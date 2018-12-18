"""Integration style tests against the files in `./sql`"""

import glob
import json

import pytest

from squabble import rules, config, lint, reporter


SQL_FILES = glob.glob('tests/sql/*.sql')
OUTPUT_MARKER = '-- >>> '


def setup_module(_mod):
    rules.load(plugins=[])


def expected_output(file_name):
    with open(file_name, 'r') as fp:
        lines = fp.readlines()

    expected = []
    for line in lines:
        if not line.startswith(OUTPUT_MARKER):
            continue

        _, out = line.split(OUTPUT_MARKER, 1)
        expected.append(json.loads(out))

    return expected


@pytest.mark.parametrize('file_name', SQL_FILES)
def test_snapshot(file_name):
    with open(file_name, 'r') as fp:
        contents = fp.read()

    expected = expected_output(file_name)

    if not expected:
        pytest.skip('no output configured')

    base_cfg = config.parse_config({})
    cfg = config.apply_file_config(base_cfg, file_name)

    issues = lint.check_file(cfg, file_name)

    assert len(issues) == len(expected)

    for i, e in zip(issues, expected):
        info = reporter._issue_info(i, contents)

        for k, v in e.items():
            assert k in info
            assert info[k] == v
