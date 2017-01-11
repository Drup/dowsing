# Dowsing

Dowsing is a type of divination employed in attempts to locate identifiers matching a given type expression.




## REFERENCES

- [RDC]:  Di Cosmo (Roberto). - Isomorphisms of types: from
  lambda-calculus to information retrieval and language
  design. - Birkhauser, 1994.

- [Rit90] Mikael Rittri. Retrieving library identifiers by
  equational matching of types in 10th Int. Conf. on Automated
  Deduction. Lecture Notes in Computer Science, 449, July
  1990.

- [Rit91]  Mikael Rittri. Using types as search keys in func-
  tion libraries. Journal of Functional Programming, 1(1):71-
  89, 1991.

### Unification

- [Rit93]  Mikael Rittri.   Retrieving library functions by
  unifying types modulo linear isomorphism.  RAIRO Theoretical
  Informatics and Applications, 27(6):523-540, 1993.

- [DJ90]

- Equality [SE94]

### With modules
- [Mul; ZW93] ADC96; ADCD97

### Database format

#### Serialization

To speed up database deserialization, we could use a format
that allows random access in an mmap datastructure.
Example of such formats:

- [Cap'n Proto](https://capnproto.org/index.html) (OCaml implementation: `capnp`)
- [Flatbuffer](https://github.com/google/flatbuffers) (No OCaml implementation)
