# hpack
Scala implementation of [RFC 7541](https://tools.ietf.org/html/rfc7541).
hpack is feature-complete, but can be further optimized. See
[Future Work](#Future Work).

## Usage
Consumers of hpack use immutable objects implementing one of six
interfaces.

### Decoder
The Decoder trait has one method: decode, which takes a Chunk[Byte] (the
input) and a DecoderContext. It returns a DecoderError if the input is
malformed or another DecoderContext if the input was well-formed.
The decode method can be called as many times as necessary with partial
input until an entire header block has been consumed. It is the
responsibility of the caller to know when that has occurred.

A reference to a Decoder can be retrieved via Decoder.default.

### DecoderContext
Once an entire header block has been passed to a Decoder, the resulting
DecoderContext will contain the uncompressed header list. DecoderContext
has one method, headerList, which returns a tuple of Seq[HeaderField] and
a new DecoderContext which can be used to decode header blocks from
subsequent requests from the same counterparty.

For the first request from an hpack counterparty, a new DecoderContext can
be had from a call to DecoderContext.default(), which takes a table size.
The correct table size must be agreed upon by both client and server via a
a negotiation which is not defined by RFC 7541.

### HeaderField
HeaderField is a case class with two fields, name and value, both of which
are Chunk[Byte]s.

### Encoder
The Encoder trait has a single method, encode, which takes a complete
header list (in the form of a Seq[HeaderField]) and an EncoderContext.
It returns a new EncoderContext that should be used for subsequent
communication with the same counterparty. An Encoder instance can be had
by calling Encoder.default.

### EncoderContext
EncoderContext is the most complicated interface in
hpack, with five methods.

#### index
The index method takes one argument, a Map[String, Indexing] that defines
indexing behavior for each field. Any field not mentioned in the mapping
will be indexed. 

#### compress
The compress method takes one argument, a Set[String], which contains the
field names that should be compressed.

#### doNotCompress
The doNotCompress method takes one argument, a Set[String], which contains the
field names that should not be compressed.

#### compressByDefault
The compressByDefault method takes one argument, a boolean, which
determines whether or not fields unmentioned in calls to compress() and
doNotCompres() should be compressed.

#### headerBlock
The headerBlock method takes no arguments and returns a tuple, a
Chunk[Byte] containing the header block and a new EncoderContext that
should be used for subsequent communication with the same counterparty.

### Indexing
Indexing is a sum type with three possible values.
1. *With* indicates that a field should be indexed.
2. *Without* indicates that a field should not be indexed.
3. *Never* indicates that a field's values contain sensitive information
and should not be indexed or otherwise cached, either by the counterparty
or by any proxies between the counterparties.

## Future Work
1. Optimize Huffman (de)compression.
2. Optimize encoding.
3. Provide a way for hpack consumers to choose between Decoder implementations.
