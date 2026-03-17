```
   ▄████████ ███▄▄▄▄      ▄████████  ▄█  ████████▄     ▄████████  ▄████████
  ███    ███ ███▀▀▀██▄   ███    ███ ███  ███   ▀███   ███    ███ ███    ███
  ███    ███ ███   ███   ███    █▀  ███▌ ███    ███   ███    █▀  ███    █▀
  ███    ███ ███   ███   ███        ███▌ ███    ███  ▄███▄▄▄     ███
▀███████████ ███   ███ ▀███████████ ███▌ ███    ███ ▀▀███▀▀▀     ███
  ███    ███ ███   ███          ███ ███  ███    ███   ███    █▄  ███    █▄
  ███    ███ ███   ███    ▄█    ███ ███  ███   ▄███   ███    ███ ███    ███
  ███    █▀   ▀█   █▀   ▄████████▀  █▀   ████████▀    ██████████ ████████▀
```

[![npm](https://img.shields.io/badge/npm-0.3.4-blue.svg)](https://www.npmjs.com/package/ansidec)
[![travis](https://travis-ci.org/jcubic/ansidec.svg?branch=master)](https://travis-ci.org/jcubic/ansidec)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/ansidec/badge.svg?branch=master)](https://coveralls.io/github/jcubic/ansidec?branch=master)
[![MIT badge](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jcubic/jquery.terminal/blob/master/LICENSE)

ANSIDec is a library for handling limited number of ANSI escape sequences for use
in Browsers. The primary goal of the library is to allow of displaying ANSI and ASCII
art in Browsers by transforming Unix encoding to html.
But it can also be used from Node.js.

## Demo

https://jcubic.github.io/ansidec/

## Installation

Npm installation for use with webpack:

```
npm install ansidec
```

Besides npm you can also download that file locally or use
[unpkg.com](https://unpkg.com/ansidec):

```html
<script src="https://unpkg.com/ansidec"></script>

```

## Usage

```javascript
// if you're using webpack or node.js you can use npm
var ansi = require('ansidec');

var format = ansi.format(function(styles, color, background, text) {
  var style = [];
  if (color) {
    style.push('color:' + color);
  }
  if (background) {
    style.push('background:' + background);
  }
  if (styles.bold) {
    style.push('font-weight:bold');
  }
  if (styles.italic) {
    style.push('font-style:italic');
  }
  if (styles.underline) {
    styles.push('text-decoration:underline');
  }
  return '<span style="' + style.join(';') + '">' + text + '</span>';
});

document.querySelector('pre').innerHTML = format(text);
```

format function can be executed with text as second argument, then it will
return string. If it don't get string as second argument it will return
function. So it's like it was curried.

If you want just to output html you can use helper:

```javascript
var ansi = require('ansidec');
var output = document.querySelector('pre');
output.innerHTML = ansi.html(text)
```

and use format only if you need different html or any different output text.

## ANSI art

If you want to render ANSI art with this library you will need to covert the text from
ANSI art file to UTF-8 to do that you can use iconv-lite library or iconv on a Back-End
see how to do that in
[examples directory](https://github.com/jcubic/ansidec/tree/master/example).

Some ANSI art are 80 characters wide but some have meta data called
[SAUCE](http://www.acid.org/info/sauce/sauce.htm). You can read those data
using `ansi.meta` function:

```javascript
var sauce = ansi.meta(text);
if (sauce) {
  var chars = sauce.tInfo[0];
  // note that ch unit don't work properly in IE
  output.style.width = chars + 'ch';
}
```

meta object have those properties (description in SAUCE specification linked above).

```typescript
{
  id: 'SAUCE',
  version: string,
  title: string,
  author: string,
  group: string,
  date: string,
  fileSize: number,
  tInfo: number[],
  comments: string,
  tflags: string,
  zstring: string
}
```

### Changelog
* 0.3.4 - fix GitHub url in README for npm
* 0.3.3 - parse zstring from SAUCE + fixes to match unit tests
* 0.3.2 - proper parsing of SAUCE comments
* 0.3.1 - alternative SAUCE extraction
* 0.3.0 - parsing SAUCE meta
* 0.2.1 - bump up version for npm
* 0.2.0 - fix 8 bit colors (Denis Ritchie)
* 0.1.0 - first version

### License

Released under [MIT](http://opensource.org/licenses/MIT) license

Copyright (c) 2018-2020 [Jakub T. Jankiewicz](https://jcubic.pl/)
