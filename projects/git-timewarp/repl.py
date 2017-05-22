from __future__ import print_function

import argparse
import readline
import sys
import traceback

import lisp


def repl(args):
    prompt = ') '
    token_stream = lisp.tokenize(sys.stdin)

    repo_path = args.git_repo or '.'

    global_scope = lisp.make_global_scope(repo_path, args.file_name)

    while True:
        try:
            sys.stderr.write(prompt)
            exp = lisp.parse(token_stream)

            if exp is None:
                continue

            if exp is lisp.EOF:
                return

            val = lisp.eval_exp(exp, global_scope)
            print('>>', val)

        except KeyboardInterrupt:
            break

        except:
            traceback.print_exc()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--git-repo', help='path to git repo')
    parser.add_argument('file_name', help='lisp file to timewarp')

    args = parser.parse_args()

    # FIXME: need to use raw_input for readline to work.
    readline.parse_and_bind('tab: complete')

    repl(args)
