# simple8601

simple8601 is an implementation of a simplified version of the ISO 8601.

**This version is not ready for production**

## What is the simplification?

* Expects only the extended format (Dates use a hyphen (`-`) separator and times a colon (`:`) separator).
* The reduced precision year will not be implemented (see [RFC3339](https://tools.ietf.org/html/rfc3339)).
* Time intervals will not be implemented.

## TODO

* Tests
* Periods
* Timezone
* Ordinal date pattern (YYYY-DDD)
* Week dates (YYYY-Www)
* Week date pattern (YYYY-Www-D)

## License

Arnau Siches under the [MIT License](https://github.com/arnau/simple8601/blob/master/LICENSE)
