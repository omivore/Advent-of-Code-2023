from solve import solve

import pytest

testdata = [
    ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19),
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23),
    ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23),
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29),
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26),
]


@pytest.mark.parametrize("stream,expected,_", testdata)
def test_detect_packet_start(stream: str, expected: int, _):
    assert solve.detect_start(stream, 4) == expected


@pytest.mark.parametrize("stream,_,expected", testdata)
def test_detect_message_start(stream: str, _, expected: int):
    assert solve.detect_start(stream, 14) == expected
