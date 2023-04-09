Gauche-rfc-punycode
===================

[![CI](https://github.com/mhayashi1120/Gauche-rfc-punycode/actions/workflows/build.yml/badge.svg)](https://github.com/mhayashi1120/Gauche-rfc-punycode/actions/workflows/build.yml)

Encode/Decode punycode for Gauche

## Install

    ./configure
    make check
    sudo make install


## Function

### punycode-encode-string / punycode-decode-string

Encode/Decode input string as punycode.
If contains invalid code in punycode, then raise error.

### punycode-encode / punycode-decode

Same as punycode-decode-string/punycode-encode-string but accept input port.

### idna-encode-string / idna-decode-string

Encode/Decode string punycode as domain name.
