Gauche-rfc-punycode
===================

Encode/Decode punycode for Gauche

## Install

    ./DIST gen
    ./configure
    make check
    sudo make install


## Function

### punycode-encode-string / punycode-decode-string

Encode/Decode input string as punycode.
If contains invalid code in punycode, then raise error.

### punycode-encode / punycode-decode

Same as punycode-decode-string/punycode-encode-string but accept input port.

### punycode-encode-idna / punycode-decode-idna

Encode/Decode string punycode as domain name.
