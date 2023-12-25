from utils import generate_diamond


def test_generate_diamond():
    assert set(generate_diamond(0)) == {(0, 0)}
    assert set(generate_diamond(1)) == {(0, 1), (1, 0), (0, -1), (-1, 0)}
