import collections
import datetime as dt
import io
import json
import logging
import os
import os.path
import re
import subprocess
import tempfile
import textwrap
import traceback

import attr

from mail2static import mailer


logger = logging.getLogger(__name__)


@attr.s
class MailData:
    sender = attr.ib()
    recipient = attr.ib()
    subject = attr.ib()
    body = attr.ib()
    attachments = attr.ib()

    site_secret = attr.ib(init=False)
    post_path = attr.ib(init=False)

    def __attrs_post_init__(self):
        (secret, path) = split_subject_line(self.subject)

        self.site_secret = secret
        self.post_path = path


def split_subject_line(subject):
    """Return tuple of (secret, post_dir). Strips out ``re:``, etc.

    >>> split_subject_line('Re: <secret> journal/story/')
    ('secret', 'journal/story/')
    >>> split_subject_line('RE: FWD: FWD: fake news')
    (None, None)
    """
    subject_line_re = re.compile(r'^.*?<([^>]+)> ([\w/]+)')
    match = re.match(subject_line_re, subject)
    if match:
        path = os.path.normpath('/' + match.group(2))[1:]
        return (match.group(1), path)

    return (None, None)


class logging_capture:
    _FORMATTER = logging.Formatter(
        '[%(asctime)s] %(name)s %(levelname)s: %(message)s')

    def __enter__(self):
        self._stream = io.StringIO()
        self._handler = logging.StreamHandler(self._stream)
        self._handler.setFormatter(self._FORMATTER)
        self._handler.setLevel(logging.DEBUG)
        logger.addHandler(self._handler)

        return self._handler

    def __exit__(self, _type, _value, _traceback):
        self._handler.flush()
        logger.removeHandler(self._handler)
        self._stream.close()


class PostManager:
    _EMAIL_TEMPLATES = {
        'created': {
            'subject': '{site_name}: post created: "{title}"',
            'body': textwrap.dedent(
                '''\
                Hi there,

                Looks like {sender} just created a new post, "{title}".

                Hopefully this was you. If it's spam, sorry.

                Full Output:
                ============\n
                {output}
                ''')
        },

        'failed': {
            'subject': '{site_name} failed to create post',
            'body': textwrap.dedent(
                '''\
                Hi there,

                Seems like that post failed to render for some reason.

                Traceback:
                ==========\n
                {traceback}


                Full Output:
                ============\n
                {output}
                ''')
        }
    }

    def __init__(self, config):
        self._config = config
        self._sites = {
            site_config['inbox_address']: SiteManager(name, site_config)
            for name, site_config
            in config.get('sites', []).items()
        }

    def _site_for_mail(self, mail_data):
        return self._sites.get(mail_data.recipient)

    def create_from_mail(self, mail_data):
        print(f'create_from_mail => {mail_data}')

        site = self._site_for_mail(mail_data)
        if not site:
            print(f'Unknown site for mail, skipping: {mail}')
            return

        if not site.is_authenticated(mail_data):
            print(f'Skipping unauthenticated mail')
            return

        self._try_create_post(site, mail_data)

    def _try_create_post(self, site, mail_data):
        with logging_capture() as log:
            try:
                title = site.create_post(mail_data)

                output = log.stream.getvalue()
                self._send_post_notification(
                    recipient=mail_data.sender,
                    template='created',
                    params={
                        'site_name': site.name,
                        'title': title,
                        'sender': mail_data.sender,
                        'output': output,
                    })

            except Exception as exc:
                logger.exception('something went wrong')
                exc_info = traceback.format_exc()

                self._send_post_notification(
                    recipient=mail_data.sender,
                    template='failed',
                    params={
                        'site_name': site.name,
                        'mail': mail_data,
                        'exception': exc,
                        'traceback': exc_info,
                        'output': log.stream.getvalue(),
                    })

    def _send_post_notification(self, recipient, template, params):
        template = self._EMAIL_TEMPLATES[template]

        mailer.send_text_email(
            self._config['smtp'],
            to_addr=recipient,
            subject=template['subject'].format(**params),
            body=template['body'].format(**params))


class SiteManager:
    def __init__(self, name, site_config):
        self.name = name
        self._config = site_config

    def create_post(self, mail_data):
        self._execute_script('before_run')

        body = self._rewrite_asset_locations(mail_data)
        markdown = _html_to_markdown(body)

        title = self._write_post(mail_data.post_path, markdown)
        self._execute_script('after_run')

        return title

    def is_authenticated(self, mail_data):
        senders = self._config.get('approved_senders', [])

        if senders and sender not in senders:
            print(f'{sender} not in whitelist for this site')
            return False

        if mail_data.site_secret != self._config['secret']:
            print('incorrect secret given for site')
            return False

        return True

    def _execute_script(self, name):
        if name not in self._config:
            return

        script = self._config[name]
        cwd = self._config['directory']

        logger.info('running "%s" script: cwd="%s", script="%s"',
                    name, cwd, script)

        proc = subprocess.run(
            script,
            stdout=subprocess.PIPE,
            universal_newlines=True,
            cwd=cwd,
            check=True,
            shell=True)

        logger.info('output:\n%s', proc.stdout)

    def _rewrite_asset_locations(self, mail_data):
        # TODO: include post title to uniqueify?
        assets_path = os.path.join(
            self._config['directory'],
            self._config['asset_base_path'],
            mail_data.post_path
        )
        logger.info('moving post assets into "%s"', assets_path)
        os.makedirs(assets_path, exist_ok=True)

        body = mail_data.body
        for cid, (temp_path, name) in mail_data.attachments.items():
            # TODO: need to sanitize name here.
            new_path = os.path.join(assets_path, name)
            os.rename(temp_path, new_path)

            logger.info('renaming %s (%s) to %s', temp_path, name, new_path)

            asset_url = os.path.join(
                '/',
                self._config['asset_base_url'],
                mail_data.post_path,
                name
            )
            body = body.replace(f'cid:{cid}', asset_url)

        return body

    def _write_post(self, post_path, markdown):
        (title, body) = _split_markdown_post_content(markdown)
        logger.info('extracted title: "%s"', title)

        posts_path = os.path.join(
            self._config['directory'],
            self._config['post_base_path'],
            post_path)

        post_path = os.path.join(
            posts_path,
            self._file_name_for_title(title))

        os.makedirs(posts_path, exist_ok=True)
        logger.info('writing post to: "%s"', post_path)

        with open(post_path, 'w') as fp:
            fp.write(self._config['post_template'].format(
                content=body,
                title=title))

        return title

    # TODO: this should be configurable
    # TODO: need to sanitize name here.
    def _file_name_for_title(self, title):
        date = dt.datetime.now().strftime('%Y-%m-%d')
        title = re.sub(r'[^\w]', '_', title)
        return f'{date}-{title}.md'


def _html_to_markdown(html):
    # TODO: make this configurable
    return subprocess.run(
        ['pandoc',
         '-f', ('html'
                '-native_spans'
                '-native_divs'),
         '-t', ('markdown'
                '-escaped_line_breaks'
                '-all_symbols_escapable'
                '-header_attributes'
                '-raw_html'
                '-auto_identifiers'
                '-link_attributes')],
        stdout=subprocess.PIPE,
        universal_newlines=True,
        input=html,
        check=True
    ).stdout


def _split_markdown_post_content(markdown):
    """Take the first non-empty line of the post as the title."""
    lines = markdown.splitlines()
    title_line = None

    for i, line in enumerate(lines):
        if line.strip() != '':
            title_line = i
            break

    if title_line is None:
        return ('untitled', '\n'.join(lines))

    return (lines[title_line], '\n'.join(lines[title_line+1:]))
