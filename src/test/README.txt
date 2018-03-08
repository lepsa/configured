We need to run Hedgehog tests to ensure that values are serialised the same
between libraries.

Additionally, we will need some IO tests that ensure when there is config from
multiple files we can generate the same config object.

This is all in addition to the obvious things like file format parsing and
exposed API types and names.
