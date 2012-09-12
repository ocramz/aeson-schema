# aeson-schema [![Build Status](https://secure.travis-ci.org/timjb/aeson-schema.png)](http://travis-ci.org/timjb/aeson-schema)

aeson-schema is an implementation of the [JSON Schema specification](http://json-schema.org). It can be used in two ways:

* To confirm that a JSON value validates against a given schema.
* To generate a parser for a schema. The generated code includes Haskell data structure definitions and FromJSON instances. This allows you to use the validated data in a type-safe and convenient way.

You can install this library using cabal:

    cabal update && cabal install aeson-schema

## Compatibility

aeson-schema implements [Draft 3](http://tools.ietf.org/html/draft-zyp-json-schema-03) of the spec. It supports all core schema definitions except the following 'format' values:

* data-time
* date
* time
* utc-millisec
* color
* style
* phone
* uri
* email
* ip-address
* ipv6
* host-name

I would be happy to accept pull requests that implement validation of one of these formats, fix bugs or add other features.