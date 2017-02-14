# readability-from-string

[![Build status][travis-image]][travis-url] [![NPM version][npm-image]][npm-url] [![XO code style][codestyle-image]][codestyle-url]

> Run [mozilla/readability](https://github.com/mozilla/readability) on an HTML string

This is just a convenience wrapper around [mozilla/readability](https://github.com/mozilla/readability).

## Installation

Install `readability-from-string` using [npm](https://www.npmjs.com/):

```bash
npm install --save readability-from-string
```

## Usage

### Module usage

```javascript
const readabilityFromString = require('readability-from-string');

const html = `
	<!DOCTYPE html>
	<html>
	  <head>
	    <meta charset="utf-8">
	    <title>Your html</title>
	  </head>
	  <body>
	    <article>
				...
			</article>
	  </body>
	</html>
`;
const result = readabilityFromString(html, {href: 'http://original.location.com/of/html.source'});
```

## API

### `readabilityFromString(html, options)`

| Name | Type | Description |
|------|------|-------------|
| html | `String` | The HTML to run Readability on |
| options | `Object` | Options |

Returns: `Object` on success or `NULL` when no content was found.

#### `options.href`

Type: `String`  
Required: `true`

Should contain the full URL for the HTML source. This is used to fix relative paths within the HTML.

## License

MIT © [Joakim Carlstein](http://joakim.beng.se)

[npm-url]: https://npmjs.org/package/readability-from-string
[npm-image]: https://badge.fury.io/js/readability-from-string.svg
[travis-url]: https://travis-ci.org/joakimbeng/readability-from-string
[travis-image]: https://travis-ci.org/joakimbeng/readability-from-string.svg?branch=master
[codestyle-url]: https://github.com/sindresorhus/xo
[codestyle-image]: https://img.shields.io/badge/code%20style-XO-5ed9c7.svg?style=flat
