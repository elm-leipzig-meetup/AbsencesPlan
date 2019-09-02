(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region._.y === region.ah.y)
	{
		return 'on line ' + region._.y;
	}
	return 'on lines ' + region._.y + ' through ' + region.ah.y;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**_UNUSED/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bh,
		impl.bE,
		impl.bz,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.a$.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done(elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done(elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.a$.b, xhr)); });
		elm$core$Maybe$isJust(request.bD) && _Http_track(router, xhr, request.bD.a);

		try {
			xhr.open(request.bm, request.bF, true);
		} catch (e) {
			return done(elm$http$Http$BadUrl_(request.bF));
		}

		_Http_configureRequest(xhr, request);

		request.aU.a && xhr.setRequestHeader('Content-Type', request.aU.a);
		xhr.send(request.aU.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.a8; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.bA.a || 0;
	xhr.responseType = request.a$.d;
	xhr.withCredentials = request.r;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? elm$http$Http$GoodStatus_ : elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		bF: xhr.responseURL,
		aK: xhr.status,
		bx: xhr.statusText,
		a8: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return elm$core$Dict$empty;
	}

	var headers = elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(elm$core$Dict$update, key, function(oldValue) {
				return elm$core$Maybe$Just(elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Sending({
			bw: event.loaded,
			Z: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Receiving({
			bu: event.loaded,
			Z: event.lengthComputable ? elm$core$Maybe$Just(event.total) : elm$core$Maybe$Nothing
		}))));
	});
}


// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2(elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2(elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}




function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2(elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		k: func(record.k),
		aa: record.aa,
		Y: record.Y
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.k;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.aa;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.Y) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bh,
		impl.bE,
		impl.bz,
		function(sendToApp, initialModel) {
			var view = impl.bH;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bh,
		impl.bE,
		impl.bz,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.z && impl.z(sendToApp)
			var view = impl.bH;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.aU);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bB) && (_VirtualDom_doc.title = title = doc.bB);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.bp;
	var onUrlRequest = impl.bq;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		z: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.az === next.az
							&& curr.al === next.al
							&& curr.aw.a === next.aw.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		bh: function(flags)
		{
			return A3(impl.bh, flags, _Browser_getUrl(), key);
		},
		bH: impl.bH,
		bE: impl.bE,
		bz: impl.bz
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { ba: 'hidden', aV: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { ba: 'mozHidden', aV: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { ba: 'msHidden', aV: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { ba: 'webkitHidden', aV: 'webkitvisibilitychange' }
		: { ba: 'hidden', aV: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		aG: _Browser_getScene(),
		aR: {
			O: _Browser_window.pageXOffset,
			P: _Browser_window.pageYOffset,
			bK: _Browser_doc.documentElement.clientWidth,
			a9: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		bK: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		a9: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			aG: {
				bK: node.scrollWidth,
				a9: node.scrollHeight
			},
			aR: {
				O: node.scrollLeft,
				P: node.scrollTop,
				bK: node.clientWidth,
				a9: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			aG: _Browser_getScene(),
			aR: {
				O: x,
				P: y,
				bK: _Browser_doc.documentElement.clientWidth,
				a9: _Browser_doc.documentElement.clientHeight
			},
			a_: {
				O: x + rect.left,
				P: y + rect.top,
				bK: rect.width,
				a9: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}
var TSFoster$elm_sha1$SHA1$Digest = F5(
	function (a, b, c, d, e) {
		return {$: 0, a: a, b: b, c: c, d: d, e: e};
	});
var TSFoster$elm_sha1$SHA1$finalDigest = function (_n0) {
	var h0 = _n0.R;
	var h1 = _n0.S;
	var h2 = _n0.T;
	var h3 = _n0.U;
	var h4 = _n0.V;
	return A5(TSFoster$elm_sha1$SHA1$Digest, h0, h1, h2, h3, h4);
};
var TSFoster$elm_sha1$SHA1$State = F5(
	function (h0, h1, h2, h3, h4) {
		return {R: h0, S: h1, T: h2, U: h3, V: h4};
	});
var TSFoster$elm_sha1$SHA1$init = A5(TSFoster$elm_sha1$SHA1$State, 1732584193, 4023233417, 2562383102, 271733878, 3285377520);
var TSFoster$elm_sha1$SHA1$DeltaState = F5(
	function (a, b, c, d, e) {
		return {E: a, F: b, G: c, H: d, I: e};
	});
var elm$core$Bitwise$and = _Bitwise_and;
var TSFoster$elm_sha1$SHA1$trim = elm$core$Bitwise$and(4294967295);
var elm$core$Basics$EQ = 1;
var elm$core$Basics$GT = 2;
var elm$core$Basics$LT = 0;
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var TSFoster$elm_sha1$SHA1$rotateLeftBy = F2(
	function (amount, i) {
		return TSFoster$elm_sha1$SHA1$trim(
			(i >>> (32 - amount)) + TSFoster$elm_sha1$SHA1$trim(i << amount));
	});
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Bitwise$complement = _Bitwise_complement;
var elm$core$Bitwise$or = _Bitwise_or;
var elm$core$Bitwise$xor = _Bitwise_xor;
var TSFoster$elm_sha1$SHA1$calculateDigestDeltas = F3(
	function (index, _int, _n0) {
		var a = _n0.E;
		var b = _n0.F;
		var c = _n0.G;
		var d = _n0.H;
		var e = _n0.I;
		var _n1 = (index < 20) ? _Utils_Tuple2(
			(b & c) | (TSFoster$elm_sha1$SHA1$trim(~b) & d),
			1518500249) : ((index < 40) ? _Utils_Tuple2(b ^ (c ^ d), 1859775393) : ((index < 60) ? _Utils_Tuple2(((b & c) | (b & d)) | (c & d), 2400959708) : _Utils_Tuple2(b ^ (c ^ d), 3395469782)));
		var f = _n1.a;
		var k = _n1.b;
		return {
			E: TSFoster$elm_sha1$SHA1$trim(
				TSFoster$elm_sha1$SHA1$trim(
					TSFoster$elm_sha1$SHA1$trim(
						TSFoster$elm_sha1$SHA1$trim(
							A2(TSFoster$elm_sha1$SHA1$rotateLeftBy, 5, a) + f) + e) + k) + _int),
			F: a,
			G: A2(TSFoster$elm_sha1$SHA1$rotateLeftBy, 30, b),
			H: c,
			I: d
		};
	});
var elm$core$Array$branchFactor = 32;
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Array$bitMask = 4294967295 >>> (32 - elm$core$Array$shiftStep);
var elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = elm$core$Array$bitMask & (index >>> shift);
			var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (!_n0.$) {
				var subTree = _n0.a;
				var $temp$shift = shift - elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _n0.a;
				return A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, values);
			}
		}
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var elm$core$Basics$ge = _Utils_ge;
var elm$core$Basics$or = _Basics_or;
var elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var elm$core$Maybe$Nothing = {$: 1};
var elm$core$Array$get = F2(
	function (index, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? elm$core$Maybe$Just(
			A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, tail)) : elm$core$Maybe$Just(
			A3(elm$core$Array$getHelp, startShift, index, tree)));
	});
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Elm$JsArray$push = _JsArray_push;
var elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					elm$core$Elm$JsArray$push,
					elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = elm$core$Array$SubTree(
					A4(elm$core$Array$insertTailInTree, shift - elm$core$Array$shiftStep, index, tail, elm$core$Elm$JsArray$empty));
				return A2(elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (!value.$) {
				var subTree = value.a;
				var newSub = elm$core$Array$SubTree(
					A4(elm$core$Array$insertTailInTree, shift - elm$core$Array$shiftStep, index, tail, subTree));
				return A3(elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = elm$core$Array$SubTree(
					A4(
						elm$core$Array$insertTailInTree,
						shift - elm$core$Array$shiftStep,
						index,
						tail,
						elm$core$Elm$JsArray$singleton(value)));
				return A3(elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		var originalTailLen = elm$core$Elm$JsArray$length(tail);
		var newTailLen = elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + elm$core$Array$shiftStep;
				var newTree = A4(
					elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					elm$core$Elm$JsArray$singleton(
						elm$core$Array$SubTree(tree)));
				return A4(elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4(elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4(elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			elm$core$Array$unsafeReplaceTail,
			A2(elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (!_n0.$) {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var TSFoster$elm_sha1$SHA1$reduceWords = F2(
	function (index, words) {
		var v = function (i) {
			return A2(elm$core$Array$get, index - i, words);
		};
		var val = A2(
			TSFoster$elm_sha1$SHA1$rotateLeftBy,
			1,
			A3(
				elm$core$List$foldl,
				elm$core$Bitwise$xor,
				0,
				A2(
					elm$core$List$filterMap,
					elm$core$Basics$identity,
					_List_fromArray(
						[
							v(3),
							v(8),
							v(14),
							v(16)
						]))));
		return A2(elm$core$Array$push, val, words);
	});
var TSFoster$elm_sha1$SHA1$wordFromInts = function (ints) {
	if ((((ints.b && ints.b.b) && ints.b.b.b) && ints.b.b.b.b) && (!ints.b.b.b.b.b)) {
		var a = ints.a;
		var _n1 = ints.b;
		var b = _n1.a;
		var _n2 = _n1.b;
		var c = _n2.a;
		var _n3 = _n2.b;
		var d = _n3.a;
		return A3(
			elm$core$List$foldl,
			elm$core$Bitwise$or,
			d,
			_List_fromArray(
				[c << 8, b << 16, a << 24]));
	} else {
		return 0;
	}
};
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.a) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.c),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.c);
		} else {
			var treeLen = builder.a * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.d) : builder.d;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.a);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.c) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.c);
		}
	});
var elm$core$Basics$True = 0;
var elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, list);
			var jsArray = _n0.a;
			var remainingItems = _n0.b;
			if (_Utils_cmp(
				elm$core$Elm$JsArray$length(jsArray),
				elm$core$Array$branchFactor) < 0) {
				return A2(
					elm$core$Array$builderToArray,
					true,
					{d: nodeList, a: nodeListSize, c: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					elm$core$List$cons,
					elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return elm$core$Array$empty;
	} else {
		return A3(elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$le = _Utils_le;
var elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2(elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var elm$core$List$takeTailRec = F2(
	function (n, list) {
		return elm$core$List$reverse(
			A3(elm$core$List$takeReverse, n, list, _List_Nil));
	});
var elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _n0 = _Utils_Tuple2(n, list);
			_n0$1:
			while (true) {
				_n0$5:
				while (true) {
					if (!_n0.b.b) {
						return list;
					} else {
						if (_n0.b.b.b) {
							switch (_n0.a) {
								case 1:
									break _n0$1;
								case 2:
									var _n2 = _n0.b;
									var x = _n2.a;
									var _n3 = _n2.b;
									var y = _n3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_n0.b.b.b.b) {
										var _n4 = _n0.b;
										var x = _n4.a;
										var _n5 = _n4.b;
										var y = _n5.a;
										var _n6 = _n5.b;
										var z = _n6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _n0$5;
									}
								default:
									if (_n0.b.b.b.b && _n0.b.b.b.b.b) {
										var _n7 = _n0.b;
										var x = _n7.a;
										var _n8 = _n7.b;
										var y = _n8.a;
										var _n9 = _n8.b;
										var z = _n9.a;
										var _n10 = _n9.b;
										var w = _n10.a;
										var tl = _n10.b;
										return (ctr > 1000) ? A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A2(elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A3(elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _n0$5;
									}
							}
						} else {
							if (_n0.a === 1) {
								break _n0$1;
							} else {
								break _n0$5;
							}
						}
					}
				}
				return list;
			}
			var _n1 = _n0.b;
			var x = _n1.a;
			return _List_fromArray(
				[x]);
		}
	});
var elm$core$List$take = F2(
	function (n, list) {
		return A3(elm$core$List$takeFast, 0, n, list);
	});
var elm_community$list_extra$List$Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var xs_ = A2(elm$core$List$drop, step, xs);
		var thisGroup = A2(elm$core$List$take, size, xs);
		var okayLength = _Utils_eq(
			size,
			elm$core$List$length(thisGroup));
		var okayArgs = (size > 0) && (step > 0);
		return (okayArgs && okayLength) ? A2(
			elm$core$List$cons,
			thisGroup,
			A3(elm_community$list_extra$List$Extra$groupsOfWithStep, size, step, xs_)) : _List_Nil;
	});
var elm_community$list_extra$List$Extra$groupsOf = F2(
	function (size, xs) {
		return A3(elm_community$list_extra$List$Extra$groupsOfWithStep, size, size, xs);
	});
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var elm_community$list_extra$List$Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _n0) {
				var i = _n0.a;
				var thisAcc = _n0.b;
				return _Utils_Tuple2(
					i + 1,
					A3(func, i, x, thisAcc));
			});
		return A3(
			elm$core$List$foldl,
			step,
			_Utils_Tuple2(0, acc),
			list).b;
	});
var elm_community$list_extra$List$Extra$initialize = F2(
	function (n, f) {
		var step = F2(
			function (i, acc) {
				step:
				while (true) {
					if (i < 0) {
						return acc;
					} else {
						var $temp$i = i - 1,
							$temp$acc = A2(
							elm$core$List$cons,
							f(i),
							acc);
						i = $temp$i;
						acc = $temp$acc;
						continue step;
					}
				}
			});
		return A2(step, n - 1, _List_Nil);
	});
var TSFoster$elm_sha1$SHA1$reduceMessage = F2(
	function (chunk, _n0) {
		var h0 = _n0.R;
		var h1 = _n0.S;
		var h2 = _n0.T;
		var h3 = _n0.U;
		var h4 = _n0.V;
		var words = elm$core$Array$fromList(
			A2(
				elm$core$List$map,
				TSFoster$elm_sha1$SHA1$wordFromInts,
				A2(elm_community$list_extra$List$Extra$groupsOf, 4, chunk)));
		var initialDeltas = A5(TSFoster$elm_sha1$SHA1$DeltaState, h0, h1, h2, h3, h4);
		var _n1 = A3(
			elm_community$list_extra$List$Extra$indexedFoldl,
			TSFoster$elm_sha1$SHA1$calculateDigestDeltas,
			initialDeltas,
			elm$core$Array$toList(
				A3(
					elm$core$List$foldl,
					TSFoster$elm_sha1$SHA1$reduceWords,
					words,
					A2(
						elm_community$list_extra$List$Extra$initialize,
						64,
						elm$core$Basics$add(16)))));
		var a = _n1.E;
		var b = _n1.F;
		var c = _n1.G;
		var d = _n1.H;
		var e = _n1.I;
		return A5(
			TSFoster$elm_sha1$SHA1$State,
			TSFoster$elm_sha1$SHA1$trim(h0 + a),
			TSFoster$elm_sha1$SHA1$trim(h1 + b),
			TSFoster$elm_sha1$SHA1$trim(h2 + c),
			TSFoster$elm_sha1$SHA1$trim(h3 + d),
			TSFoster$elm_sha1$SHA1$trim(h4 + e));
	});
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$modBy = _Basics_modBy;
var elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2(elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var elm$core$List$repeat = F2(
	function (n, value) {
		return A3(elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var TSFoster$elm_sha1$SHA1$hashBytes = function (bytes) {
	var byteCount = elm$core$List$length(bytes);
	var zeroBytesToAppend = 4 + A2(
		elm$core$Basics$modBy,
		64,
		56 - A2(elm$core$Basics$modBy, 64, byteCount + 1));
	var bitCountInBytes = _List_fromArray(
		[255 & (byteCount >>> (24 - 3)), 255 & (byteCount >>> (16 - 3)), 255 & (byteCount >>> (8 - 3)), 255 & (byteCount << 3)]);
	var bytesToAppend = A2(
		elm$core$List$cons,
		128,
		_Utils_ap(
			A2(elm$core$List$repeat, zeroBytesToAppend, 0),
			bitCountInBytes));
	var message = _Utils_ap(bytes, bytesToAppend);
	var chunks = A2(elm_community$list_extra$List$Extra$groupsOf, 64, message);
	var hashState = A3(elm$core$List$foldl, TSFoster$elm_sha1$SHA1$reduceMessage, TSFoster$elm_sha1$SHA1$init, chunks);
	return TSFoster$elm_sha1$SHA1$finalDigest(hashState);
};
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$Char$toCode = _Char_toCode;
var elm$core$String$foldl = _String_foldl;
var zwilias$elm_utf_tools$String$UTF8$utf32ToUtf8 = F3(
	function (add, _char, acc) {
		return (_char < 128) ? A2(add, _char, acc) : ((_char < 2048) ? A2(
			add,
			128 | (63 & _char),
			A2(add, 192 | (_char >>> 6), acc)) : ((_char < 65536) ? A2(
			add,
			128 | (63 & _char),
			A2(
				add,
				128 | (63 & (_char >>> 6)),
				A2(add, 224 | (_char >>> 12), acc))) : A2(
			add,
			128 | (63 & _char),
			A2(
				add,
				128 | (63 & (_char >>> 6)),
				A2(
					add,
					128 | (63 & (_char >>> 12)),
					A2(add, 240 | (_char >>> 18), acc))))));
	});
var zwilias$elm_utf_tools$String$UTF8$foldl = F3(
	function (op, initialAcc, input) {
		return A3(
			elm$core$String$foldl,
			F2(
				function (_char, acc) {
					return A3(
						zwilias$elm_utf_tools$String$UTF8$utf32ToUtf8,
						op,
						elm$core$Char$toCode(_char),
						acc);
				}),
			initialAcc,
			input);
	});
var zwilias$elm_utf_tools$String$UTF8$toBytes = function (input) {
	return elm$core$List$reverse(
		A3(zwilias$elm_utf_tools$String$UTF8$foldl, elm$core$List$cons, _List_Nil, input));
};
var TSFoster$elm_sha1$SHA1$fromString = A2(elm$core$Basics$composeR, zwilias$elm_utf_tools$String$UTF8$toBytes, TSFoster$elm_sha1$SHA1$hashBytes);
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var TSFoster$elm_sha1$SHA1$base64Chars = elm$core$Array$fromList(
	elm$core$String$toList('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'));
var elm$core$String$fromList = _String_fromList;
var TSFoster$elm_sha1$SHA1$intToBase64 = function (_int) {
	return elm$core$String$fromList(
		A2(
			elm$core$List$filterMap,
			elm$core$Basics$apR(TSFoster$elm_sha1$SHA1$base64Chars),
			A2(
				elm$core$List$map,
				elm$core$Array$get,
				_List_fromArray(
					[63 & (_int >>> 18), 63 & (_int >>> 12), 63 & (_int >>> 6), 63 & _int]))));
};
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$core$String$slice = _String_slice;
var elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3(elm$core$String$slice, 0, -n, string);
	});
var TSFoster$elm_sha1$SHA1$toBase64 = function (_n0) {
	var a = _n0.a;
	var b = _n0.b;
	var c = _n0.c;
	var d = _n0.d;
	var e = _n0.e;
	return function (s) {
		return s + '=';
	}(
		A2(
			elm$core$String$dropRight,
			1,
			elm$core$String$concat(
				A2(
					elm$core$List$map,
					TSFoster$elm_sha1$SHA1$intToBase64,
					_List_fromArray(
						[a >>> 8, ((255 & a) << 16) + (b >>> 16), ((65535 & b) << 8) + (c >>> 24), 16777215 & c, d >>> 8, ((255 & d) << 16) + (e >>> 16), (65535 & e) << 8])))));
};
var TSFoster$elm_uuid$UUID$UUID = elm$core$Basics$identity;
var TSFoster$elm_uuid$UUID$setVariantBits = function (v) {
	switch (v) {
		case 1:
			return A2(
				elm$core$Basics$composeR,
				elm$core$Bitwise$and(63),
				elm$core$Bitwise$or(128));
		case 2:
			return A2(
				elm$core$Basics$composeR,
				elm$core$Bitwise$and(31),
				elm$core$Bitwise$or(192));
		default:
			return elm$core$Basics$identity;
	}
};
var elm_community$list_extra$List$Extra$updateAt = F3(
	function (index, fn, list) {
		if (index < 0) {
			return list;
		} else {
			var tail = A2(elm$core$List$drop, index, list);
			var head = A2(elm$core$List$take, index, list);
			if (tail.b) {
				var x = tail.a;
				var xs = tail.b;
				return _Utils_ap(
					head,
					A2(
						elm$core$List$cons,
						fn(x),
						xs));
			} else {
				return list;
			}
		}
	});
var TSFoster$elm_uuid$UUID$toVariant = F2(
	function (v, _n0) {
		var bytes = _n0;
		return A3(
			elm_community$list_extra$List$Extra$updateAt,
			8,
			TSFoster$elm_uuid$UUID$setVariantBits(v),
			bytes);
	});
var TSFoster$elm_uuid$UUID$toVersion = F2(
	function (v, _n0) {
		var bytes = _n0;
		var updateFn = A2(
			elm_community$list_extra$List$Extra$updateAt,
			6,
			A2(
				elm$core$Basics$composeR,
				elm$core$Bitwise$and(15),
				elm$core$Bitwise$or((15 & v) << 4)));
		return updateFn(bytes);
	});
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$random$Random$Generator = elm$core$Basics$identity;
var elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$random$Random$next = function (_n0) {
	var state0 = _n0.a;
	var incr = _n0.b;
	return A2(elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var elm$random$Random$peel = function (_n0) {
	var state = _n0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var elm$random$Random$int = F2(
	function (a, b) {
		return function (seed0) {
			var _n0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
			var lo = _n0.a;
			var hi = _n0.b;
			var range = (hi - lo) + 1;
			if (!((range - 1) & range)) {
				return _Utils_Tuple2(
					(((range - 1) & elm$random$Random$peel(seed0)) >>> 0) + lo,
					elm$random$Random$next(seed0));
			} else {
				var threshhold = (((-range) >>> 0) % range) >>> 0;
				var accountForBias = function (seed) {
					accountForBias:
					while (true) {
						var x = elm$random$Random$peel(seed);
						var seedN = elm$random$Random$next(seed);
						if (_Utils_cmp(x, threshhold) < 0) {
							var $temp$seed = seedN;
							seed = $temp$seed;
							continue accountForBias;
						} else {
							return _Utils_Tuple2((x % range) + lo, seedN);
						}
					}
				};
				return accountForBias(seed0);
			}
		};
	});
var elm$random$Random$listHelp = F4(
	function (revList, n, gen, seed) {
		listHelp:
		while (true) {
			if (n < 1) {
				return _Utils_Tuple2(revList, seed);
			} else {
				var _n0 = gen(seed);
				var value = _n0.a;
				var newSeed = _n0.b;
				var $temp$revList = A2(elm$core$List$cons, value, revList),
					$temp$n = n - 1,
					$temp$gen = gen,
					$temp$seed = newSeed;
				revList = $temp$revList;
				n = $temp$n;
				gen = $temp$gen;
				seed = $temp$seed;
				continue listHelp;
			}
		}
	});
var elm$random$Random$list = F2(
	function (n, _n0) {
		var gen = _n0;
		return function (seed) {
			return A4(elm$random$Random$listHelp, _List_Nil, n, gen, seed);
		};
	});
var elm$random$Random$map = F2(
	function (func, _n0) {
		var genA = _n0;
		return function (seed0) {
			var _n1 = genA(seed0);
			var a = _n1.a;
			var seed1 = _n1.b;
			return _Utils_Tuple2(
				func(a),
				seed1);
		};
	});
var TSFoster$elm_uuid$UUID$generator = A2(
	elm$random$Random$map,
	TSFoster$elm_uuid$UUID$toVariant(1),
	A2(
		elm$random$Random$map,
		TSFoster$elm_uuid$UUID$toVersion(4),
		A2(
			elm$random$Random$map,
			elm$core$Basics$identity,
			A2(
				elm$random$Random$list,
				16,
				A2(elm$random$Random$int, 0, 255)))));
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var elm$core$String$length = _String_length;
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				elm$core$String$repeat,
				n - elm$core$String$length(string),
				elm$core$String$fromChar(_char)),
			string);
	});
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm_community$string_extra$String$Extra$breaker = F3(
	function (width, string, acc) {
		breaker:
		while (true) {
			if (string === '') {
				return elm$core$List$reverse(acc);
			} else {
				var $temp$width = width,
					$temp$string = A2(elm$core$String$dropLeft, width, string),
					$temp$acc = A2(
					elm$core$List$cons,
					A3(elm$core$String$slice, 0, width, string),
					acc);
				width = $temp$width;
				string = $temp$string;
				acc = $temp$acc;
				continue breaker;
			}
		}
	});
var elm_community$string_extra$String$Extra$break = F2(
	function (width, string) {
		return ((!width) || (string === '')) ? _List_fromArray(
			[string]) : A3(elm_community$string_extra$String$Extra$breaker, width, string, _List_Nil);
	});
var elm$core$Basics$idiv = _Basics_idiv;
var rtfeldman$elm_hex$Hex$unsafeToDigit = function (num) {
	unsafeToDigit:
	while (true) {
		switch (num) {
			case 0:
				return '0';
			case 1:
				return '1';
			case 2:
				return '2';
			case 3:
				return '3';
			case 4:
				return '4';
			case 5:
				return '5';
			case 6:
				return '6';
			case 7:
				return '7';
			case 8:
				return '8';
			case 9:
				return '9';
			case 10:
				return 'a';
			case 11:
				return 'b';
			case 12:
				return 'c';
			case 13:
				return 'd';
			case 14:
				return 'e';
			case 15:
				return 'f';
			default:
				var $temp$num = num;
				num = $temp$num;
				continue unsafeToDigit;
		}
	}
};
var rtfeldman$elm_hex$Hex$unsafePositiveToDigits = F2(
	function (digits, num) {
		unsafePositiveToDigits:
		while (true) {
			if (num < 16) {
				return A2(
					elm$core$List$cons,
					rtfeldman$elm_hex$Hex$unsafeToDigit(num),
					digits);
			} else {
				var $temp$digits = A2(
					elm$core$List$cons,
					rtfeldman$elm_hex$Hex$unsafeToDigit(
						A2(elm$core$Basics$modBy, 16, num)),
					digits),
					$temp$num = (num / 16) | 0;
				digits = $temp$digits;
				num = $temp$num;
				continue unsafePositiveToDigits;
			}
		}
	});
var rtfeldman$elm_hex$Hex$toString = function (num) {
	return elm$core$String$fromList(
		(num < 0) ? A2(
			elm$core$List$cons,
			'-',
			A2(rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, -num)) : A2(rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, num));
};
var TSFoster$elm_uuid$UUID$canonical = function (_n0) {
	var bytes = _n0;
	var strings = A2(
		elm_community$string_extra$String$Extra$break,
		4,
		elm$core$String$concat(
			A2(
				elm$core$List$map,
				A2(elm$core$String$padLeft, 2, '0'),
				A2(elm$core$List$map, rtfeldman$elm_hex$Hex$toString, bytes))));
	if ((((((((strings.b && strings.b.b) && strings.b.b.b) && strings.b.b.b.b) && strings.b.b.b.b.b) && strings.b.b.b.b.b.b) && strings.b.b.b.b.b.b.b) && strings.b.b.b.b.b.b.b.b) && (!strings.b.b.b.b.b.b.b.b.b)) {
		var a = strings.a;
		var _n2 = strings.b;
		var b = _n2.a;
		var _n3 = _n2.b;
		var c = _n3.a;
		var _n4 = _n3.b;
		var d = _n4.a;
		var _n5 = _n4.b;
		var e = _n5.a;
		var _n6 = _n5.b;
		var f = _n6.a;
		var _n7 = _n6.b;
		var g = _n7.a;
		var _n8 = _n7.b;
		var h = _n8.a;
		return a + (b + ('-' + (c + ('-' + (d + ('-' + (e + ('-' + (f + (g + h))))))))));
	} else {
		return 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx';
	}
};
var TSFoster$elm_uuid$UUID$toString = TSFoster$elm_uuid$UUID$canonical;
var author$project$Devs$Objects$PublicHoliday = F2(
	function (title, date) {
		return {aX: date, bB: title};
	});
var elm$core$Basics$False = 1;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{d: nodeList, a: (len / elm$core$Array$branchFactor) | 0, c: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 1) {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$string = _Json_decodeString;
var author$project$Devs$DatabaseDecode$pubHolDecoder = A3(
	elm$json$Json$Decode$map2,
	author$project$Devs$Objects$PublicHoliday,
	A2(elm$json$Json$Decode$field, 'title', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'date', elm$json$Json$Decode$string));
var elm$json$Json$Decode$list = _Json_decodeList;
var author$project$Devs$DatabaseDecode$pubHolListDecoder = elm$json$Json$Decode$list(author$project$Devs$DatabaseDecode$pubHolDecoder);
var author$project$Devs$Objects$PubHolYear = F3(
	function (year, fedState, pubHolList) {
		return {aj: fedState, aA: pubHolList, aS: year};
	});
var elm$json$Json$Decode$int = _Json_decodeInt;
var elm$json$Json$Decode$map3 = _Json_map3;
var author$project$Devs$DatabaseDecode$pubHolYearDecoder = A4(
	elm$json$Json$Decode$map3,
	author$project$Devs$Objects$PubHolYear,
	A2(elm$json$Json$Decode$field, 'year', elm$json$Json$Decode$int),
	A2(elm$json$Json$Decode$field, 'fedState', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'pubHolList', author$project$Devs$DatabaseDecode$pubHolListDecoder));
var author$project$Devs$DatabaseDecode$pubHolYearListDecoder = elm$json$Json$Decode$list(author$project$Devs$DatabaseDecode$pubHolYearDecoder);
var author$project$Devs$Objects$Config = F8(
	function (maxHoliday, maxLearningHoliday, holidayURL, fedState, random, password, loggedIn, pubHolList) {
		return {aj: fedState, be: holidayURL, bi: loggedIn, bk: maxHoliday, bl: maxLearningHoliday, bs: password, aA: pubHolList, bt: random};
	});
var elm$json$Json$Decode$bool = _Json_decodeBool;
var elm$json$Json$Decode$map8 = _Json_map8;
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$oneOf = _Json_oneOf;
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$json$Json$Decode$maybe = function (decoder) {
	return elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(elm$json$Json$Decode$map, elm$core$Maybe$Just, decoder),
				elm$json$Json$Decode$succeed(elm$core$Maybe$Nothing)
			]));
};
var author$project$Devs$DatabaseDecode$configDecoder = A9(
	elm$json$Json$Decode$map8,
	author$project$Devs$Objects$Config,
	A2(elm$json$Json$Decode$field, 'maxHoliday', elm$json$Json$Decode$int),
	A2(elm$json$Json$Decode$field, 'maxLearningHoliday', elm$json$Json$Decode$int),
	A2(elm$json$Json$Decode$field, 'holidayURL', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'fedState', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'random', elm$json$Json$Decode$int),
	elm$json$Json$Decode$maybe(
		A2(elm$json$Json$Decode$field, 'password', elm$json$Json$Decode$string)),
	A2(elm$json$Json$Decode$field, 'loggedIn', elm$json$Json$Decode$bool),
	A2(elm$json$Json$Decode$field, 'pubHolList', author$project$Devs$DatabaseDecode$pubHolYearListDecoder));
var author$project$Devs$Objects$Date = F3(
	function (day, month, year) {
		return {aY: day, bn: month, aS: year};
	});
var author$project$Devs$DatabaseDecode$dateDecoder = A4(
	elm$json$Json$Decode$map3,
	author$project$Devs$Objects$Date,
	A2(elm$json$Json$Decode$field, 'day', elm$json$Json$Decode$int),
	A2(elm$json$Json$Decode$field, 'month', elm$json$Json$Decode$int),
	A2(elm$json$Json$Decode$field, 'year', elm$json$Json$Decode$int));
var author$project$Devs$Objects$Holiday = F4(
	function (from, to, holType, uuid) {
		return {a2: from, bc: holType, bC: to, bG: uuid};
	});
var elm$json$Json$Decode$map4 = _Json_map4;
var author$project$Devs$DatabaseDecode$holDecoder = A5(
	elm$json$Json$Decode$map4,
	author$project$Devs$Objects$Holiday,
	A2(elm$json$Json$Decode$field, 'from', author$project$Devs$DatabaseDecode$dateDecoder),
	A2(elm$json$Json$Decode$field, 'to', author$project$Devs$DatabaseDecode$dateDecoder),
	A2(elm$json$Json$Decode$field, 'holType', elm$json$Json$Decode$int),
	A2(elm$json$Json$Decode$field, 'uuid', elm$json$Json$Decode$string));
var author$project$Devs$DatabaseDecode$holListDecoder = elm$json$Json$Decode$list(author$project$Devs$DatabaseDecode$holDecoder);
var author$project$Devs$Objects$TransferObj = F3(
	function (config, holList, init) {
		return {aW: config, bb: holList, bh: init};
	});
var author$project$Devs$DatabaseDecode$dbDecoder = A4(
	elm$json$Json$Decode$map3,
	author$project$Devs$Objects$TransferObj,
	A2(elm$json$Json$Decode$field, 'config', author$project$Devs$DatabaseDecode$configDecoder),
	A2(elm$json$Json$Decode$field, 'holList', author$project$Devs$DatabaseDecode$holListDecoder),
	A2(elm$json$Json$Decode$field, 'init', elm$json$Json$Decode$bool));
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$json$Json$Encode$null = _Json_encodeNull;
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$Devs$DatabaseEncode$encodeString = function (str) {
	if (!str.$) {
		var val = str.a;
		return elm$core$String$isEmpty(val) ? elm$json$Json$Encode$null : elm$json$Json$Encode$string(val);
	} else {
		return elm$json$Json$Encode$null;
	}
};
var elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			elm$core$List$foldl,
			F2(
				function (_n0, obj) {
					var k = _n0.a;
					var v = _n0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var author$project$Devs$DatabaseEncode$publicHolidayEncoder = function (obj) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'title',
				elm$json$Json$Encode$string(obj.bB)),
				_Utils_Tuple2(
				'date',
				elm$json$Json$Encode$string(obj.aX))
			]));
};
var elm$json$Json$Encode$int = _Json_wrap;
var elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var author$project$Devs$DatabaseEncode$pubHolYearEncoder = function (obj) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'year',
				elm$json$Json$Encode$int(obj.aS)),
				_Utils_Tuple2(
				'fedState',
				elm$json$Json$Encode$string(obj.aj)),
				_Utils_Tuple2(
				'pubHolList',
				A2(elm$json$Json$Encode$list, author$project$Devs$DatabaseEncode$publicHolidayEncoder, obj.aA))
			]));
};
var elm$json$Json$Encode$bool = _Json_wrap;
var author$project$Devs$DatabaseEncode$configEncoder = function (obj) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'maxHoliday',
				elm$json$Json$Encode$int(obj.bk)),
				_Utils_Tuple2(
				'maxLearningHoliday',
				elm$json$Json$Encode$int(obj.bl)),
				_Utils_Tuple2(
				'holidayURL',
				elm$json$Json$Encode$string(obj.be)),
				_Utils_Tuple2(
				'fedState',
				elm$json$Json$Encode$string(obj.aj)),
				_Utils_Tuple2(
				'random',
				elm$json$Json$Encode$int(obj.bt)),
				_Utils_Tuple2(
				'password',
				author$project$Devs$DatabaseEncode$encodeString(obj.bs)),
				_Utils_Tuple2(
				'loggedIn',
				elm$json$Json$Encode$bool(obj.bi)),
				_Utils_Tuple2(
				'pubHolList',
				A2(elm$json$Json$Encode$list, author$project$Devs$DatabaseEncode$pubHolYearEncoder, obj.aA))
			]));
};
var author$project$Devs$DatabaseEncode$dateEncoder = function (obj) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'day',
				elm$json$Json$Encode$int(obj.aY)),
				_Utils_Tuple2(
				'month',
				elm$json$Json$Encode$int(obj.bn)),
				_Utils_Tuple2(
				'year',
				elm$json$Json$Encode$int(obj.aS))
			]));
};
var author$project$Devs$DatabaseEncode$holidayEncoder = function (obj) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'from',
				author$project$Devs$DatabaseEncode$dateEncoder(obj.a2)),
				_Utils_Tuple2(
				'to',
				author$project$Devs$DatabaseEncode$dateEncoder(obj.bC)),
				_Utils_Tuple2(
				'holType',
				elm$json$Json$Encode$int(obj.bc)),
				_Utils_Tuple2(
				'uuid',
				elm$json$Json$Encode$string(obj.bG))
			]));
};
var author$project$Devs$DatabaseEncode$dbEncoder = function (obj) {
	var list = _List_fromArray(
		[
			_Utils_Tuple2(
			'config',
			author$project$Devs$DatabaseEncode$configEncoder(obj.aW)),
			_Utils_Tuple2(
			'holList',
			A2(elm$json$Json$Encode$list, author$project$Devs$DatabaseEncode$holidayEncoder, obj.bb)),
			_Utils_Tuple2(
			'init',
			elm$json$Json$Encode$bool(obj.bh))
		]);
	return elm$json$Json$Encode$object(list);
};
var TSFoster$elm_uuid$UUID$nil = A2(elm$core$List$repeat, 16, 0);
var author$project$Devs$Objects$getEpmtyHoliday = {
	a2: {aY: 0, bn: 0, aS: 0},
	bc: 2,
	bC: {aY: 0, bn: 0, aS: 0},
	bG: TSFoster$elm_uuid$UUID$toString(TSFoster$elm_uuid$UUID$nil)
};
var author$project$Devs$Ports$getDimOfElement = _Platform_outgoingPort('getDimOfElement', elm$json$Json$Encode$string);
var elm$core$Maybe$destruct = F3(
	function (_default, func, maybe) {
		if (!maybe.$) {
			var a = maybe.a;
			return func(a);
		} else {
			return _default;
		}
	});
var author$project$Devs$Ports$pushDataToStore = _Platform_outgoingPort(
	'pushDataToStore',
	function ($) {
		return elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'config',
					function ($) {
						return elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'fedState',
									elm$json$Json$Encode$string($.aj)),
									_Utils_Tuple2(
									'holidayURL',
									elm$json$Json$Encode$string($.be)),
									_Utils_Tuple2(
									'loggedIn',
									elm$json$Json$Encode$bool($.bi)),
									_Utils_Tuple2(
									'maxHoliday',
									elm$json$Json$Encode$int($.bk)),
									_Utils_Tuple2(
									'maxLearningHoliday',
									elm$json$Json$Encode$int($.bl)),
									_Utils_Tuple2(
									'password',
									function ($) {
										return A3(elm$core$Maybe$destruct, elm$json$Json$Encode$null, elm$json$Json$Encode$string, $);
									}($.bs)),
									_Utils_Tuple2(
									'pubHolList',
									elm$json$Json$Encode$list(
										function ($) {
											return elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'fedState',
														elm$json$Json$Encode$string($.aj)),
														_Utils_Tuple2(
														'pubHolList',
														elm$json$Json$Encode$list(
															function ($) {
																return elm$json$Json$Encode$object(
																	_List_fromArray(
																		[
																			_Utils_Tuple2(
																			'date',
																			elm$json$Json$Encode$string($.aX)),
																			_Utils_Tuple2(
																			'title',
																			elm$json$Json$Encode$string($.bB))
																		]));
															})($.aA)),
														_Utils_Tuple2(
														'year',
														elm$json$Json$Encode$int($.aS))
													]));
										})($.aA)),
									_Utils_Tuple2(
									'random',
									elm$json$Json$Encode$int($.bt))
								]));
					}($.aW)),
					_Utils_Tuple2(
					'holList',
					elm$json$Json$Encode$list(
						function ($) {
							return elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'from',
										function ($) {
											return elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'day',
														elm$json$Json$Encode$int($.aY)),
														_Utils_Tuple2(
														'month',
														elm$json$Json$Encode$int($.bn)),
														_Utils_Tuple2(
														'year',
														elm$json$Json$Encode$int($.aS))
													]));
										}($.a2)),
										_Utils_Tuple2(
										'holType',
										elm$json$Json$Encode$int($.bc)),
										_Utils_Tuple2(
										'to',
										function ($) {
											return elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'day',
														elm$json$Json$Encode$int($.aY)),
														_Utils_Tuple2(
														'month',
														elm$json$Json$Encode$int($.bn)),
														_Utils_Tuple2(
														'year',
														elm$json$Json$Encode$int($.aS))
													]));
										}($.bC)),
										_Utils_Tuple2(
										'uuid',
										elm$json$Json$Encode$string($.bG))
									]));
						})($.bb)),
					_Utils_Tuple2(
					'init',
					elm$json$Json$Encode$bool($.bh))
				]));
	});
var author$project$Devs$TypeObject$DBDecode = function (a) {
	return {$: 25, a: a};
};
var author$project$Devs$TypeObject$DBLoaded = function (a) {
	return {$: 24, a: a};
};
var author$project$Devs$TypeObject$SetTimeZone = function (a) {
	return {$: 5, a: a};
};
var author$project$Devs$TypeObject$SetYear = function (a) {
	return {$: 6, a: a};
};
var AdrianRibao$elm_derberos_date$Derberos$Date$Core$monthToNumber1based = function (month) {
	switch (month) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
			return 5;
		case 5:
			return 6;
		case 6:
			return 7;
		case 7:
			return 8;
		case 8:
			return 9;
		case 9:
			return 10;
		case 10:
			return 11;
		default:
			return 12;
	}
};
var AdrianRibao$elm_derberos_date$Derberos$Date$Core$newDateRecord = F8(
	function (year, month, day, hour, minute, second, millis, zone) {
		return {aY: day, J: hour, K: millis, L: minute, bn: month, N: second, aS: year, Q: zone};
	});
var elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$time$Time$customZone = elm$time$Time$Zone;
var elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return elm$core$Basics$floor(numerator / denominator);
	});
var elm$time$Time$posixToMillis = function (_n0) {
	var millis = _n0;
	return millis;
};
var elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era._, posixMinutes) < 0) {
					return posixMinutes + era.ar;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var elm$time$Time$toAdjustedMinutes = F2(
	function (_n0, time) {
		var defaultOffset = _n0.a;
		var eras = _n0.b;
		return A3(
			elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				elm$time$Time$flooredDiv,
				elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2(elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		aY: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		bn: month,
		aS: year + ((month <= 2) ? 1 : 0)
	};
};
var elm$time$Time$toDay = F2(
	function (zone, time) {
		return elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).aY;
	});
var elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			elm$core$Basics$modBy,
			24,
			A2(
				elm$time$Time$flooredDiv,
				A2(elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var elm$time$Time$toMillis = F2(
	function (_n0, time) {
		return A2(
			elm$core$Basics$modBy,
			1000,
			elm$time$Time$posixToMillis(time));
	});
var elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			elm$core$Basics$modBy,
			60,
			A2(elm$time$Time$toAdjustedMinutes, zone, time));
	});
var elm$time$Time$Apr = 3;
var elm$time$Time$Aug = 7;
var elm$time$Time$Dec = 11;
var elm$time$Time$Feb = 1;
var elm$time$Time$Jan = 0;
var elm$time$Time$Jul = 6;
var elm$time$Time$Jun = 5;
var elm$time$Time$Mar = 2;
var elm$time$Time$May = 4;
var elm$time$Time$Nov = 10;
var elm$time$Time$Oct = 9;
var elm$time$Time$Sep = 8;
var elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _n0 = elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).bn;
		switch (_n0) {
			case 1:
				return 0;
			case 2:
				return 1;
			case 3:
				return 2;
			case 4:
				return 3;
			case 5:
				return 4;
			case 6:
				return 5;
			case 7:
				return 6;
			case 8:
				return 7;
			case 9:
				return 8;
			case 10:
				return 9;
			case 11:
				return 10;
			default:
				return 11;
		}
	});
var elm$time$Time$toSecond = F2(
	function (_n0, time) {
		return A2(
			elm$core$Basics$modBy,
			60,
			A2(
				elm$time$Time$flooredDiv,
				elm$time$Time$posixToMillis(time),
				1000));
	});
var elm$time$Time$toYear = F2(
	function (zone, time) {
		return elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).aS;
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilFromPosixWithTimezone = F2(
	function (tz, time) {
		var zeroOffset = A2(elm$time$Time$customZone, 0, _List_Nil);
		var year = A2(elm$time$Time$toYear, tz, time);
		var second = A2(elm$time$Time$toSecond, tz, time);
		var month = AdrianRibao$elm_derberos_date$Derberos$Date$Core$monthToNumber1based(
			A2(elm$time$Time$toMonth, tz, time));
		var minute = A2(elm$time$Time$toMinute, tz, time);
		var millis = A2(elm$time$Time$toMillis, tz, time);
		var hour = A2(elm$time$Time$toHour, tz, time);
		var day = A2(elm$time$Time$toDay, tz, time);
		return A8(AdrianRibao$elm_derberos_date$Derberos$Date$Core$newDateRecord, year, month, day, hour, minute, second, millis, zeroOffset);
	});
var elm$time$Time$Posix = elm$core$Basics$identity;
var elm$time$Time$millisToPosix = elm$core$Basics$identity;
var AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosixUnadjusted = function (dateRecord) {
	var y = dateRecord.aS - ((dateRecord.bn <= 2) ? 1 : 0);
	var time = ((((dateRecord.J * 3600) * 1000) + ((dateRecord.L * 60) * 1000)) + (dateRecord.N * 1000)) + dateRecord.K;
	var mp = A2(elm$core$Basics$modBy, 12, dateRecord.bn + 9);
	var era = elm$core$Basics$floor(y / 400);
	var yoe = y - (era * 400);
	var doy = (((((153 * mp) + 2) / 5) | 0) + dateRecord.aY) - 1;
	var doe = (((yoe * 365) + ((yoe / 4) | 0)) - ((yoe / 100) | 0)) + doy;
	var days = ((era * 146097) + doe) - 719468;
	var resultInMilliseconds = (((days * 24) * 3600) * 1000) + time;
	return elm$time$Time$millisToPosix(resultInMilliseconds);
};
var AdrianRibao$elm_derberos_date$Derberos$Date$Core$getTzOffset = F2(
	function (zone, time) {
		var utcMillis = elm$time$Time$posixToMillis(time);
		var localMillis = elm$time$Time$posixToMillis(
			AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosixUnadjusted(
				A2(AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilFromPosixWithTimezone, zone, time)));
		return ((localMillis - utcMillis) / 60000) | 0;
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Core$adjustMilliseconds = F2(
	function (zone, time) {
		var offset = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Core$getTzOffset, zone, time);
		var millis = elm$time$Time$posixToMillis(time);
		return elm$time$Time$millisToPosix(millis - (offset * 60000));
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosix = function (dateRecord) {
	return A2(
		AdrianRibao$elm_derberos_date$Derberos$Date$Core$adjustMilliseconds,
		dateRecord.Q,
		AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosixUnadjusted(dateRecord));
};
var AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$setDay1OfMonth = function (civilTime) {
	return _Utils_update(
		civilTime,
		{aY: 1, J: 0, K: 0, L: 0, N: 0});
};
var AdrianRibao$elm_derberos_date$Derberos$Date$Core$addTimezoneMilliseconds = F2(
	function (zone, time) {
		var offset = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Core$getTzOffset, zone, time);
		var millis = elm$time$Time$posixToMillis(time);
		return elm$time$Time$millisToPosix(millis + (offset * 60000));
	});
var elm$time$Time$utc = A2(elm$time$Time$Zone, 0, _List_Nil);
var AdrianRibao$elm_derberos_date$Derberos$Date$Core$posixToCivil = function (time) {
	var milliseconds = elm$time$Time$posixToMillis(time);
	var minute = A2(
		elm$core$Basics$modBy,
		60,
		elm$core$Basics$floor(milliseconds / (60 * 1000)));
	var minutes = elm$core$Basics$floor(milliseconds / (60 * 1000));
	var rawDay = elm$core$Basics$floor((minutes / (60 * 24)) + 719468);
	var second = A2(
		elm$core$Basics$modBy,
		60,
		elm$core$Basics$floor(milliseconds / 1000));
	var millis = A2(elm$core$Basics$modBy, 1000, milliseconds);
	var hour = A2(
		elm$core$Basics$modBy,
		24,
		elm$core$Basics$floor(milliseconds / ((60 * 60) * 1000)));
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		aY: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		J: hour,
		K: millis,
		L: minute,
		bn: month,
		N: second,
		aS: year + ((month <= 2) ? 1 : 0),
		Q: elm$time$Time$utc
	};
};
var AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getFirstDayOfMonth = F2(
	function (zone, time) {
		return A2(
			AdrianRibao$elm_derberos_date$Derberos$Date$Core$adjustMilliseconds,
			zone,
			AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosix(
				AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$setDay1OfMonth(
					AdrianRibao$elm_derberos_date$Derberos$Date$Core$posixToCivil(
						A2(AdrianRibao$elm_derberos_date$Derberos$Date$Core$addTimezoneMilliseconds, zone, time)))));
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Delta$addDays = F2(
	function (delta, time) {
		return elm$time$Time$millisToPosix(
			((((delta * 1000) * 60) * 60) * 24) + elm$time$Time$posixToMillis(time));
	});
var elm$core$Basics$neq = _Utils_notEqual;
var AdrianRibao$elm_derberos_date$Derberos$Date$Utils$isLeapYear = function (year) {
	return (((!A2(elm$core$Basics$modBy, 4, year)) && A2(elm$core$Basics$modBy, 100, year)) || (!A2(elm$core$Basics$modBy, 400, year))) ? true : false;
};
var AdrianRibao$elm_derberos_date$Derberos$Date$Utils$numberOfDaysInMonth = F2(
	function (year, month) {
		switch (month) {
			case 0:
				return 31;
			case 1:
				var _n1 = AdrianRibao$elm_derberos_date$Derberos$Date$Utils$isLeapYear(year);
				if (_n1) {
					return 29;
				} else {
					return 28;
				}
			case 2:
				return 31;
			case 3:
				return 30;
			case 4:
				return 31;
			case 5:
				return 30;
			case 6:
				return 31;
			case 7:
				return 31;
			case 8:
				return 30;
			case 9:
				return 31;
			case 10:
				return 30;
			default:
				return 31;
		}
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Utils$numberToMonth = function (monthNumber) {
	switch (monthNumber) {
		case 0:
			return elm$core$Maybe$Just(0);
		case 1:
			return elm$core$Maybe$Just(1);
		case 2:
			return elm$core$Maybe$Just(2);
		case 3:
			return elm$core$Maybe$Just(3);
		case 4:
			return elm$core$Maybe$Just(4);
		case 5:
			return elm$core$Maybe$Just(5);
		case 6:
			return elm$core$Maybe$Just(6);
		case 7:
			return elm$core$Maybe$Just(7);
		case 8:
			return elm$core$Maybe$Just(8);
		case 9:
			return elm$core$Maybe$Just(9);
		case 10:
			return elm$core$Maybe$Just(10);
		case 11:
			return elm$core$Maybe$Just(11);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getCurrentMonthDates = F2(
	function (zone, time) {
		var firstDayOfMonth = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getFirstDayOfMonth, zone, time);
		var dateRecord = AdrianRibao$elm_derberos_date$Derberos$Date$Core$posixToCivil(time);
		var month = A2(
			elm$core$Maybe$withDefault,
			0,
			AdrianRibao$elm_derberos_date$Derberos$Date$Utils$numberToMonth(dateRecord.bn - 1));
		var numberDaysInMonth = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Utils$numberOfDaysInMonth, dateRecord.aS, month);
		var year = dateRecord.aS;
		return A2(
			elm$core$List$map,
			function (delta) {
				return A2(AdrianRibao$elm_derberos_date$Derberos$Date$Delta$addDays, delta, firstDayOfMonth);
			},
			A2(elm$core$List$range, 0, numberDaysInMonth - 1));
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getLastDayOfMonth = F2(
	function (zone, time) {
		var dateRecord = AdrianRibao$elm_derberos_date$Derberos$Date$Core$posixToCivil(
			A2(AdrianRibao$elm_derberos_date$Derberos$Date$Core$addTimezoneMilliseconds, zone, time));
		var month = A2(
			elm$core$Maybe$withDefault,
			0,
			AdrianRibao$elm_derberos_date$Derberos$Date$Utils$numberToMonth(dateRecord.bn - 1));
		var year = dateRecord.aS;
		var lastDayInMonth = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Utils$numberOfDaysInMonth, year, month);
		var newRecord = _Utils_update(
			dateRecord,
			{aY: lastDayInMonth, J: 0, K: 0, L: 0, N: 0});
		return A2(
			AdrianRibao$elm_derberos_date$Derberos$Date$Core$adjustMilliseconds,
			zone,
			AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosix(newRecord));
	});
var elm$time$Time$Fri = 4;
var elm$time$Time$Mon = 0;
var elm$time$Time$Sat = 5;
var elm$time$Time$Sun = 6;
var elm$time$Time$Thu = 3;
var elm$time$Time$Tue = 1;
var elm$time$Time$Wed = 2;
var AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayFromNumber = function (weekdayNumber) {
	switch (weekdayNumber) {
		case 0:
			return elm$core$Maybe$Just(0);
		case 1:
			return elm$core$Maybe$Just(1);
		case 2:
			return elm$core$Maybe$Just(2);
		case 3:
			return elm$core$Maybe$Just(3);
		case 4:
			return elm$core$Maybe$Just(4);
		case 5:
			return elm$core$Maybe$Just(5);
		case 6:
			return elm$core$Maybe$Just(6);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var AdrianRibao$elm_derberos_date$Derberos$Date$Utils$getWeekday = F2(
	function (zone, time) {
		var offset = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Core$getTzOffset, zone, time);
		var milliseconds = elm$time$Time$posixToMillis(time);
		var adjustMilliseconds = milliseconds + (offset * 60000);
		var days = elm$core$Basics$floor(adjustMilliseconds / (((24 * 60) * 60) * 1000));
		var weekdayNumber = A2(elm$core$Basics$modBy, 7, days + 3);
		return A2(
			elm$core$Maybe$withDefault,
			0,
			AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayFromNumber(weekdayNumber));
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayToNumber = function (weekday) {
	switch (weekday) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		case 3:
			return 3;
		case 4:
			return 4;
		case 5:
			return 5;
		default:
			return 6;
	}
};
var AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayDiff = F2(
	function (day1, day2) {
		var day2Number = AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayToNumber(day2);
		var day1Number = AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayToNumber(day1);
		return (day2Number - day1Number) + ((_Utils_cmp(day1Number, day2Number) < 1) ? 0 : 7);
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Delta$nextWeekdayFromTime = F3(
	function (weekday, zone, time) {
		var timeWeekday = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Utils$getWeekday, zone, time);
		var diffDays = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayDiff, timeWeekday, weekday);
		return A2(AdrianRibao$elm_derberos_date$Derberos$Date$Delta$addDays, diffDays, time);
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayDiffBack = F2(
	function (day1, day2) {
		var day2Number = AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayToNumber(day2);
		var day1Number = AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayToNumber(day1);
		return ((_Utils_cmp(day1Number, day2Number) > -1) ? 0 : 7) - (day2Number - day1Number);
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Delta$prevWeekdayFromTime = F3(
	function (weekday, zone, time) {
		var timeWeekday = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Utils$getWeekday, zone, time);
		var diffDays = (-1) * A2(AdrianRibao$elm_derberos_date$Derberos$Date$Utils$weekdayDiffBack, timeWeekday, weekday);
		return A2(AdrianRibao$elm_derberos_date$Derberos$Date$Delta$addDays, diffDays, time);
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getCurrentMonthDatesFullWeeks = F2(
	function (zone, time) {
		var lastDayOfMonth = A3(
			AdrianRibao$elm_derberos_date$Derberos$Date$Delta$nextWeekdayFromTime,
			6,
			zone,
			A2(AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getLastDayOfMonth, zone, time));
		var firstDayOfMonth = A3(
			AdrianRibao$elm_derberos_date$Derberos$Date$Delta$prevWeekdayFromTime,
			0,
			zone,
			A2(AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getFirstDayOfMonth, zone, time));
		var numberDaysInMonth = ((elm$time$Time$posixToMillis(lastDayOfMonth) - elm$time$Time$posixToMillis(firstDayOfMonth)) / (((1000 * 60) * 60) * 24)) | 0;
		return A2(
			elm$core$List$map,
			function (delta) {
				return A2(AdrianRibao$elm_derberos_date$Derberos$Date$Delta$addDays, delta, firstDayOfMonth);
			},
			A2(elm$core$List$range, 0, numberDaysInMonth));
	});
var AdrianRibao$elm_derberos_date$Derberos$Date$Utils$resetTime = function (time) {
	return elm$time$Time$millisToPosix(
		(((1000 * 60) * 60) * 24) * function (millis) {
			return (millis / (((1000 * 60) * 60) * 24)) | 0;
		}(
			elm$time$Time$posixToMillis(time)));
};
var AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getCurrentWeekDates = F2(
	function (zone, time) {
		var weekMonday = A3(
			AdrianRibao$elm_derberos_date$Derberos$Date$Delta$prevWeekdayFromTime,
			0,
			zone,
			A2(
				AdrianRibao$elm_derberos_date$Derberos$Date$Core$adjustMilliseconds,
				zone,
				AdrianRibao$elm_derberos_date$Derberos$Date$Utils$resetTime(
					A2(AdrianRibao$elm_derberos_date$Derberos$Date$Core$addTimezoneMilliseconds, zone, time))));
		return A2(
			elm$core$List$map,
			function (delta) {
				return A2(AdrianRibao$elm_derberos_date$Derberos$Date$Delta$addDays, delta, weekMonday);
			},
			A2(elm$core$List$range, 0, 6));
	});
var author$project$Devs$Utils$dateToCivil = F2(
	function (zone, d) {
		return A8(AdrianRibao$elm_derberos_date$Derberos$Date$Core$newDateRecord, d.aS, d.bn, d.aY, 15, 0, 0, 0, zone);
	});
var author$project$Devs$Utils$getDiffOfTS = F2(
	function (date1, date2) {
		return ((((((((date2 - date1) / 1000) | 0) / 60) | 0) / 60) | 0) / 24) | 0;
	});
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var author$project$Devs$Utils$getWeek = F3(
	function (fDoY, zone, weekDays) {
		var fWDoW = function () {
			var _n1 = elm$core$List$head(weekDays);
			if (!_n1.$) {
				var wd = _n1.a;
				return wd;
			} else {
				return {
					aY: {
						aX: {aY: fDoY.aY, bn: fDoY.bn, aS: fDoY.aS},
						bc: elm$core$Maybe$Nothing,
						bd: elm$core$Maybe$Nothing
					},
					M: ''
				};
			}
		}();
		var date2 = elm$time$Time$posixToMillis(
			AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosix(
				A2(author$project$Devs$Utils$dateToCivil, zone, fWDoW.aY.aX)));
		var date1 = function () {
			var _n0 = elm$core$List$head(
				A2(
					AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getCurrentWeekDates,
					zone,
					AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosix(fDoY)));
			if (!_n0.$) {
				var date = _n0.a;
				return elm$time$Time$posixToMillis(date);
			} else {
				return 0;
			}
		}();
		var diff = (A2(author$project$Devs$Utils$getDiffOfTS, date1, date2) / 7) | 0;
		return {aq: diff + 1, bI: weekDays};
	});
var author$project$Devs$Utils$daterecordToString = function (d) {
	return elm$core$String$fromInt(d.aS) + ('-' + (A3(
		elm$core$String$padLeft,
		2,
		'0',
		elm$core$String$fromInt(d.bn)) + ('-' + A3(
		elm$core$String$padLeft,
		2,
		'0',
		elm$core$String$fromInt(d.aY)))));
};
var author$project$Devs$Utils$civilToDate = function (d) {
	return {aY: d.aY, bn: d.bn, aS: d.aS};
};
var author$project$Devs$Objects$Hol = 2;
var author$project$Devs$Objects$Ill = 0;
var author$project$Devs$Objects$Kar = 1;
var author$project$Devs$Objects$LHol = 3;
var author$project$Devs$Objects$getDefaultHoliday = 2;
var author$project$Devs$Utils$intToHoltype = function (idx) {
	return (!idx) ? 0 : ((idx === 1) ? 1 : ((idx === 2) ? 2 : ((idx === 3) ? 3 : author$project$Devs$Objects$getDefaultHoliday)));
};
var elm_community$list_extra$List$Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var author$project$Devs$Utils$getHoltypeFromDate = F3(
	function (date, zone, hol) {
		var posix2 = AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosix(
			A2(author$project$Devs$Utils$dateToCivil, zone, hol.bC));
		var posix1 = AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosix(
			A2(author$project$Devs$Utils$dateToCivil, zone, hol.a2));
		var diff = A2(
			author$project$Devs$Utils$getDiffOfTS,
			elm$time$Time$posixToMillis(posix1),
			elm$time$Time$posixToMillis(posix2));
		var holList = A2(
			elm$core$List$map,
			function (i) {
				return author$project$Devs$Utils$civilToDate(
					AdrianRibao$elm_derberos_date$Derberos$Date$Core$posixToCivil(
						A2(AdrianRibao$elm_derberos_date$Derberos$Date$Delta$addDays, i, posix1)));
			},
			A2(elm$core$List$range, 0, diff));
		var _n0 = A2(
			elm_community$list_extra$List$Extra$find,
			function (a) {
				return _Utils_eq(a.aY, date.aY) && (_Utils_eq(a.bn, date.bn) && _Utils_eq(a.aS, date.aS));
			},
			holList);
		if (!_n0.$) {
			var h = _n0.a;
			return elm$core$Maybe$Just(
				author$project$Devs$Utils$intToHoltype(hol.bc));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var author$project$Devs$Utils$getHoldayTypeOfDate = F3(
	function (date, hList, zone) {
		var holList = A2(
			elm$core$List$filter,
			function (a) {
				return _Utils_eq(a.a2.aS, date.aS) || _Utils_eq(a.bC.aS, date.aS);
			},
			hList);
		var holTypeList = A2(
			elm$core$List$filter,
			function (a) {
				return !_Utils_eq(a, elm$core$Maybe$Nothing);
			},
			A2(
				elm$core$List$map,
				A2(author$project$Devs$Utils$getHoltypeFromDate, date, zone),
				holList));
		return A2(
			elm$core$Maybe$withDefault,
			elm$core$Maybe$Nothing,
			elm$core$List$head(holTypeList));
	});
var author$project$Devs$Utils$holTypeToInt = function (hType) {
	switch (hType) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var author$project$Devs$Utils$posixToDay = F4(
	function (pHolidays, holidays, zone, day) {
		var d = AdrianRibao$elm_derberos_date$Derberos$Date$Core$posixToCivil(day);
		var date = {aY: d.aY, bn: d.bn, aS: d.aS};
		var hd = A3(author$project$Devs$Utils$getHoldayTypeOfDate, d, holidays, zone);
		var ht = function () {
			if (!hd.$) {
				var h = hd.a;
				return elm$core$Maybe$Just(
					author$project$Devs$Utils$holTypeToInt(h));
			} else {
				return elm$core$Maybe$Nothing;
			}
		}();
		var phd = function () {
			var _n0 = A2(
				elm_community$list_extra$List$Extra$find,
				function (a) {
					return _Utils_eq(
						a.aX,
						author$project$Devs$Utils$daterecordToString(d));
				},
				pHolidays);
			if (!_n0.$) {
				var ph = _n0.a;
				return elm$core$Maybe$Just(ph.bB);
			} else {
				return elm$core$Maybe$Nothing;
			}
		}();
		return {aX: date, bc: ht, bd: phd};
	});
var author$project$Devs$Utils$toGermanWeekday = function (wd) {
	switch (wd) {
		case 0:
			return 'Mo';
		case 1:
			return 'Di';
		case 2:
			return 'Mi';
		case 3:
			return 'Do';
		case 4:
			return 'Fr';
		case 5:
			return 'Sa';
		default:
			return 'So';
	}
};
var author$project$Devs$Utils$posixToWeekday = F4(
	function (pHolidays, holidays, zone, day) {
		var dName = author$project$Devs$Utils$toGermanWeekday(
			A2(AdrianRibao$elm_derberos_date$Derberos$Date$Utils$getWeekday, zone, day));
		var d = A4(author$project$Devs$Utils$posixToDay, pHolidays, holidays, zone, day);
		return {aY: d, M: dName};
	});
var author$project$Devs$Utils$toGermanMonth = function (month) {
	switch (month) {
		case 0:
			return 'Januar';
		case 1:
			return 'Februar';
		case 2:
			return 'März';
		case 4:
			return 'Mai';
		case 3:
			return 'April';
		case 5:
			return 'Juni';
		case 6:
			return 'Juli';
		case 7:
			return 'August';
		case 8:
			return 'September';
		case 9:
			return 'Oktober';
		case 10:
			return 'November';
		default:
			return 'Dezember';
	}
};
var author$project$Devs$Utils$getMonth = F5(
	function (zone, fDoY, pHolidays, holidays, monthNr) {
		var z = zone;
		var monthName = function () {
			var _n0 = AdrianRibao$elm_derberos_date$Derberos$Date$Utils$numberToMonth(monthNr);
			if (!_n0.$) {
				var m = _n0.a;
				return author$project$Devs$Utils$toGermanMonth(m);
			} else {
				return 'noMonth';
			}
		}();
		var d = A8(AdrianRibao$elm_derberos_date$Derberos$Date$Core$newDateRecord, fDoY.aS, monthNr + 1, 3, 15, 0, 0, 0, z);
		var date = AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosix(d);
		var dayList = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getCurrentMonthDates, z, date);
		var days = A2(
			elm$core$List$map,
			A3(author$project$Devs$Utils$posixToDay, pHolidays, holidays, z),
			dayList);
		var weekList = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Calendar$getCurrentMonthDatesFullWeeks, z, date);
		var weekDays = A2(
			elm$core$List$map,
			A3(author$project$Devs$Utils$posixToWeekday, pHolidays, holidays, z),
			weekList);
		var weeks = A2(
			elm$core$List$map,
			A2(author$project$Devs$Utils$getWeek, fDoY, z),
			A2(elm_community$list_extra$List$Extra$groupsOf, 7, weekDays));
		return {aZ: days, M: monthName, aq: monthNr, bJ: weeks};
	});
var author$project$Devs$Utils$getMonths = F4(
	function (fDoY, zone, pHolidays, holidays) {
		return A2(
			elm$core$List$map,
			A4(author$project$Devs$Utils$getMonth, zone, fDoY, pHolidays, holidays),
			A2(elm$core$List$range, 0, 11));
	});
var author$project$Devs$Utils$fillYear = F4(
	function (year, zone, pHolidays, holidays) {
		var z = elm$time$Time$utc;
		var fDoY = A8(AdrianRibao$elm_derberos_date$Derberos$Date$Core$newDateRecord, year, 1, 1, 1, 0, 0, 0, z);
		var fDoYP = AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosix(fDoY);
		var months = A4(author$project$Devs$Utils$getMonths, fDoY, z, pHolidays, holidays);
		return {ao: months, M: year};
	});
var author$project$Devs$Utils$getPubHolListOfYear = F3(
	function (pubHolYearList, currentYear, fedState) {
		var _n0 = elm$core$List$head(
			A2(
				elm$core$List$filter,
				function (phy) {
					return _Utils_eq(phy.aS, currentYear) && _Utils_eq(phy.aj, fedState);
				},
				pubHolYearList));
		if (!_n0.$) {
			var phy = _n0.a;
			return phy.aA;
		} else {
			return _List_Nil;
		}
	});
var elm$random$Random$initialSeed = function (x) {
	var _n0 = elm$random$Random$next(
		A2(elm$random$Random$Seed, 0, 1013904223));
	var state1 = _n0.a;
	var incr = _n0.b;
	var state2 = (state1 + x) >>> 0;
	return elm$random$Random$next(
		A2(elm$random$Random$Seed, state2, incr));
};
var author$project$Devs$Utils$getSeed = function (model) {
	var _n0 = model.af;
	if (!_n0.$) {
		var seed = _n0.a;
		return seed;
	} else {
		return elm$random$Random$initialSeed(model.aW.bt);
	}
};
var author$project$Devs$TypeObject$SetPublicHolidays = function (a) {
	return {$: 7, a: a};
};
var author$project$Devs$Decode$holidayDecoder = A3(
	elm$json$Json$Decode$map2,
	author$project$Devs$Objects$PublicHoliday,
	A2(elm$json$Json$Decode$field, 'title', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'date', elm$json$Json$Decode$string));
var elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Dict$Black = 1;
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Dict$Red = 0;
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1) {
				case 0:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr === 1) {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr === 1) {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === -1) {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === -1) {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === -1) {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (!_n0.$) {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var elm$http$Http$NetworkError_ = {$: 2};
var elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$Timeout_ = {$: 1};
var elm$http$Http$emptyBody = _Http_emptyBody;
var elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2(elm$json$Json$Encode$encode, 0, value));
};
var elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$http$Http$State = F2(
	function (reqs, subs) {
		return {aD: reqs, aL: subs};
	});
var elm$http$Http$init = elm$core$Task$succeed(
	A2(elm$http$Http$State, elm$core$Dict$empty, _List_Nil));
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Process$kill = _Scheduler_kill;
var elm$core$Process$spawn = _Scheduler_spawn;
var elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _n2 = A2(elm$core$Dict$get, tracker, reqs);
					if (_n2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _n2.a;
						return A2(
							elm$core$Task$andThen,
							function (_n3) {
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2(elm$core$Dict$remove, tracker, reqs));
							},
							elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						elm$core$Task$andThen,
						function (pid) {
							var _n4 = req.bD;
							if (_n4.$ === 1) {
								return A3(elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _n4.a;
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3(elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			elm$core$Task$andThen,
			function (reqs) {
				return elm$core$Task$succeed(
					A2(elm$http$Http$State, reqs, subs));
			},
			A3(elm$http$Http$updateReqs, router, cmds, state.aD));
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _n0) {
		var actualTracker = _n0.a;
		var toMsg = _n0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? elm$core$Maybe$Just(
			A2(
				elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : elm$core$Maybe$Nothing;
	});
var elm$http$Http$onSelfMsg = F3(
	function (router, _n0, state) {
		var tracker = _n0.a;
		var progress = _n0.b;
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$filterMap,
					A3(elm$http$Http$maybeSend, router, tracker, progress),
					state.aL)));
	});
var elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return elm$http$Http$Request(
				{
					r: r.r,
					aU: r.aU,
					a$: A2(_Http_mapExpect, func, r.a$),
					a8: r.a8,
					bm: r.bm,
					bA: r.bA,
					bD: r.bD,
					bF: r.bF
				});
		}
	});
var elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$http$Http$subMap = F2(
	function (func, _n0) {
		var tracker = _n0.a;
		var toMsg = _n0.b;
		return A2(
			elm$http$Http$MySub,
			tracker,
			A2(elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager(elm$http$Http$init, elm$http$Http$onEffects, elm$http$Http$onSelfMsg, elm$http$Http$cmdMap, elm$http$Http$subMap);
var elm$http$Http$command = _Platform_leaf('Http');
var elm$http$Http$subscription = _Platform_leaf('Http');
var elm$http$Http$request = function (r) {
	return elm$http$Http$command(
		elm$http$Http$Request(
			{r: false, aU: r.aU, a$: r.a$, a8: r.a8, bm: r.bm, bA: r.bA, bD: r.bD, bF: r.bF}));
};
var author$project$Devs$Utils$myRequest = F4(
	function (method, url, expect, content) {
		var header = _List_Nil;
		var body = function () {
			if (!content.$) {
				var reqBody = content.a;
				return elm$http$Http$jsonBody(reqBody);
			} else {
				return elm$http$Http$emptyBody;
			}
		}();
		return elm$http$Http$request(
			{aU: body, a$: expect, a8: header, bm: method, bA: elm$core$Maybe$Nothing, bD: elm$core$Maybe$Nothing, bF: url});
	});
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			elm$core$Basics$identity,
			A2(elm$core$Basics$composeR, toResult, toMsg));
	});
var elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$NetworkError = {$: 2};
var elm$http$Http$Timeout = {$: 1};
var elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadUrl(url));
			case 1:
				return elm$core$Result$Err(elm$http$Http$Timeout);
			case 2:
				return elm$core$Result$Err(elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadStatus(metadata.aK));
			default:
				var body = response.b;
				return A2(
					elm$core$Result$mapError,
					elm$http$Http$BadBody,
					toResult(body));
		}
	});
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			elm$http$Http$expectStringResponse,
			toMsg,
			elm$http$Http$resolve(
				function (string) {
					return A2(
						elm$core$Result$mapError,
						elm$json$Json$Decode$errorToString,
						A2(elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var author$project$Devs$Utils$setPublicHolidaysApi = F2(
	function (event, url) {
		return A4(
			author$project$Devs$Utils$myRequest,
			'GET',
			url,
			A2(
				elm$http$Http$expectJson,
				event,
				elm$json$Json$Decode$list(author$project$Devs$Decode$holidayDecoder)),
			elm$core$Maybe$Nothing);
	});
var elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			elm$core$String$join,
			after,
			A2(elm$core$String$split, before, string));
	});
var author$project$Devs$Utils$setPublicHolidays = function (model) {
	var url1 = A3(
		elm$core$String$replace,
		'[year]',
		elm$core$String$fromInt(model.ag),
		model.aW.be);
	var url2 = A3(elm$core$String$replace, '[fedState]', model.aW.aj, url1);
	return A2(author$project$Devs$Utils$setPublicHolidaysApi, author$project$Devs$TypeObject$SetPublicHolidays, url2);
};
var elm$core$String$toInt = _String_toInt;
var elm_community$list_extra$List$Extra$getAt = F2(
	function (idx, xs) {
		return (idx < 0) ? elm$core$Maybe$Nothing : elm$core$List$head(
			A2(elm$core$List$drop, idx, xs));
	});
var author$project$Devs$Utils$stringToDate = function (str) {
	var strArr = A2(elm$core$String$split, '-', str);
	var year = A2(
		elm$core$Maybe$withDefault,
		0,
		elm$core$String$toInt(
			A2(
				elm$core$Maybe$withDefault,
				'0',
				A2(elm_community$list_extra$List$Extra$getAt, 0, strArr))));
	var month = A2(
		elm$core$Maybe$withDefault,
		0,
		elm$core$String$toInt(
			A2(
				elm$core$Maybe$withDefault,
				'0',
				A2(elm_community$list_extra$List$Extra$getAt, 1, strArr))));
	var day = A2(
		elm$core$Maybe$withDefault,
		0,
		elm$core$String$toInt(
			A2(
				elm$core$Maybe$withDefault,
				'0',
				A2(elm_community$list_extra$List$Extra$getAt, 2, strArr))));
	return {aY: day, bn: month, aS: year};
};
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var author$project$Devs$Utils$updatePublicHolidayListInConfig = F2(
	function (config, pubHolYear) {
		return (elm$core$List$length(
			A2(
				elm$core$List$filter,
				function (item) {
					return _Utils_eq(item.aS, pubHolYear.aS);
				},
				config.aA)) > 0) ? config : _Utils_update(
			config,
			{
				aA: A2(
					elm$core$List$append,
					config.aA,
					_List_fromArray(
						[pubHolYear]))
			});
	});
var elm$core$Basics$not = _Basics_not;
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var elm$core$Task$Perform = elm$core$Basics$identity;
var elm$core$Task$init = elm$core$Task$succeed(0);
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return 0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0;
		return A2(elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			A2(elm$core$Task$map, toMessage, task));
	});
var elm$file$File$toString = _File_toString;
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$file$File$Download$string = F3(
	function (name, mime, content) {
		return A2(
			elm$core$Task$perform,
			elm$core$Basics$never,
			A3(_File_download, name, mime, content));
	});
var elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var elm$random$Random$step = F2(
	function (_n0, seed) {
		var generator = _n0;
		return generator(seed);
	});
var elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var elm$time$Time$here = _Time_here(0);
var elm$time$Time$now = _Time_now(elm$time$Time$millisToPosix);
var author$project$Devs$Update$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 1:
				var val = msg.a;
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 2:
				var val = msg.a;
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 26:
				var id = msg.a;
				return _Utils_Tuple2(
					model,
					author$project$Devs$Ports$getDimOfElement(id));
			case 27:
				var dim = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{as: dim.a9, at: dim.bK}),
					elm$core$Platform$Cmd$none);
			case 3:
				var obj = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aW: obj.aW, bb: obj.bb}),
					A2(elm$core$Task$perform, author$project$Devs$TypeObject$SetTimeZone, elm$time$Time$here));
			case 12:
				var val = msg.a;
				var c = model.aW;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aW: _Utils_update(
								c,
								{
									bk: A2(
										elm$core$Maybe$withDefault,
										0,
										elm$core$String$toInt(val))
								})
						}),
					elm$core$Platform$Cmd$none);
			case 13:
				var val = msg.a;
				var c = model.aW;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aW: _Utils_update(
								c,
								{
									bl: A2(
										elm$core$Maybe$withDefault,
										0,
										elm$core$String$toInt(val))
								})
						}),
					elm$core$Platform$Cmd$none);
			case 14:
				var val = msg.a;
				var c = model.aW;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aW: _Utils_update(
								c,
								{aj: val})
						}),
					elm$core$Platform$Cmd$none);
			case 15:
				var val = msg.a;
				var c = model.aW;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aW: _Utils_update(
								c,
								{be: val})
						}),
					elm$core$Platform$Cmd$none);
			case 16:
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aP: elm$core$String$isEmpty(val) ? elm$core$Maybe$Nothing : elm$core$Maybe$Just(val)
						}),
					elm$core$Platform$Cmd$none);
			case 4:
				var op = msg.a;
				var newYear = function () {
					if (!op) {
						return model.ag + 1;
					} else {
						return model.ag - 1;
					}
				}();
				var m = _Utils_update(
					model,
					{ag: newYear});
				return _Utils_Tuple2(
					m,
					author$project$Devs$Utils$setPublicHolidays(m));
			case 5:
				var zone = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aN: zone}),
					A2(elm$core$Task$perform, author$project$Devs$TypeObject$SetYear, elm$time$Time$now));
			case 6:
				var ts = msg.a;
				var m = (model.ag > 1977) ? model : _Utils_update(
					model,
					{
						ag: A2(elm$time$Time$toYear, model.aN, ts)
					});
				return _Utils_Tuple2(
					m,
					author$project$Devs$Utils$setPublicHolidays(m));
			case 17:
				var val = msg.a;
				var tmpHol = function () {
					var _n2 = model.aO;
					if (!_n2.$) {
						var holiday = _n2.a;
						return holiday;
					} else {
						return author$project$Devs$Objects$getEpmtyHoliday;
					}
				}();
				var hol = _Utils_update(
					tmpHol,
					{
						a2: author$project$Devs$Utils$stringToDate(val)
					});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aO: elm$core$Maybe$Just(hol)
						}),
					elm$core$Platform$Cmd$none);
			case 18:
				var val = msg.a;
				var tmpHol = function () {
					var _n3 = model.aO;
					if (!_n3.$) {
						var holiday = _n3.a;
						return holiday;
					} else {
						return author$project$Devs$Objects$getEpmtyHoliday;
					}
				}();
				var hol = _Utils_update(
					tmpHol,
					{
						bC: author$project$Devs$Utils$stringToDate(val)
					});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aO: elm$core$Maybe$Just(hol)
						}),
					elm$core$Platform$Cmd$none);
			case 19:
				var val = msg.a;
				var tmpHol = function () {
					var _n4 = model.aO;
					if (!_n4.$) {
						var holiday = _n4.a;
						return holiday;
					} else {
						return author$project$Devs$Objects$getEpmtyHoliday;
					}
				}();
				var holType = author$project$Devs$Utils$intToHoltype(
					A2(
						elm$core$Maybe$withDefault,
						0,
						elm$core$String$toInt(val)));
				var hol = _Utils_update(
					tmpHol,
					{
						bc: author$project$Devs$Utils$holTypeToInt(holType)
					});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aO: elm$core$Maybe$Just(hol)
						}),
					elm$core$Platform$Cmd$none);
			case 20:
				var _n5 = A2(
					elm$random$Random$step,
					TSFoster$elm_uuid$UUID$generator,
					author$project$Devs$Utils$getSeed(model));
				var newUuid = _n5.a;
				var newSeed = _n5.b;
				var _n6 = model.aO;
				if (!_n6.$) {
					var holiday = _n6.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								af: elm$core$Maybe$Just(newSeed),
								bb: A2(
									elm$core$List$append,
									model.bb,
									_List_fromArray(
										[
											_Utils_update(
											holiday,
											{
												bG: TSFoster$elm_uuid$UUID$toString(newUuid)
											})
										])),
								aO: elm$core$Maybe$Nothing
							}),
						elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
			case 21:
				var uuid = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							bb: A2(
								elm$core$List$filter,
								function (i) {
									return !_Utils_eq(i.bG, uuid);
								},
								model.bb)
						}),
					elm$core$Platform$Cmd$none);
			case 7:
				if (!msg.a.$) {
					var list = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								ac: A4(author$project$Devs$Utils$fillYear, model.ag, model.aN, list, model.bb),
								aW: A2(
									author$project$Devs$Utils$updatePublicHolidayListInConfig,
									model.aW,
									{aj: model.aW.aj, aA: list, aS: model.ag})
							}),
						elm$core$Platform$Cmd$none);
				} else {
					var error = msg.a.a;
					var publicHolidaysFromCache = A3(author$project$Devs$Utils$getPubHolListOfYear, model.aW.aA, model.ag, model.aW.aj);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								ac: A4(author$project$Devs$Utils$fillYear, model.ag, model.aN, publicHolidaysFromCache, model.bb)
							}),
						elm$core$Platform$Cmd$none);
				}
			case 8:
				var c = model.aW;
				var c1 = ((!_Utils_eq(model.aP, elm$core$Maybe$Nothing)) && (!elm$core$String$isEmpty(
					A2(elm$core$Maybe$withDefault, '', model.aP)))) ? _Utils_update(
					c,
					{
						bs: elm$core$Maybe$Just(
							TSFoster$elm_sha1$SHA1$toBase64(
								TSFoster$elm_sha1$SHA1$fromString(
									A2(elm$core$Maybe$withDefault, '', model.aP))))
					}) : c;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aW: c1, aH: !model.aH, aP: elm$core$Maybe$Nothing}),
					elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								author$project$Devs$Ports$pushDataToStore(
								{aW: c1, bb: model.bb, bh: false}),
								author$project$Devs$Utils$setPublicHolidays(model)
							])));
			case 9:
				var hash = ((!_Utils_eq(model.aP, elm$core$Maybe$Nothing)) && (!elm$core$String$isEmpty(
					A2(elm$core$Maybe$withDefault, '', model.aP)))) ? TSFoster$elm_sha1$SHA1$toBase64(
					TSFoster$elm_sha1$SHA1$fromString(
						A2(elm$core$Maybe$withDefault, '', model.aP))) : '';
				var c = model.aW;
				var c1 = _Utils_eq(
					hash,
					A2(elm$core$Maybe$withDefault, '', model.aW.bs)) ? _Utils_update(
					c,
					{bi: true}) : c;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aW: c1, aJ: !model.aJ, aP: elm$core$Maybe$Nothing}),
					author$project$Devs$Ports$pushDataToStore(
						{aW: c1, bb: model.bb, bh: false}));
			case 10:
				var c = model.aW;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							aW: _Utils_update(
								c,
								{bi: !model.aW.bi})
						}),
					author$project$Devs$Ports$pushDataToStore(
						{
							aW: _Utils_update(
								c,
								{bi: !model.aW.bi}),
							bb: model.bb,
							bh: false
						}));
			case 11:
				var tmpHol = (!model.aI) ? elm$core$Maybe$Just(author$project$Devs$Objects$getEpmtyHoliday) : elm$core$Maybe$Nothing;
				var cmds = model.aI ? elm$core$Platform$Cmd$batch(
					_List_fromArray(
						[
							author$project$Devs$Ports$pushDataToStore(
							{aW: model.aW, bb: model.bb, bh: false}),
							author$project$Devs$Utils$setPublicHolidays(model)
						])) : elm$core$Platform$Cmd$none;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aI: !model.aI, aO: tmpHol}),
					cmds);
			case 22:
				return _Utils_Tuple2(
					model,
					A3(
						elm$file$File$Download$string,
						'holidayDB.json',
						'application/json',
						A2(
							elm$json$Json$Encode$encode,
							0,
							author$project$Devs$DatabaseEncode$dbEncoder(
								{aW: model.aW, bb: model.bb, bh: false}))));
			case 23:
				return _Utils_Tuple2(
					model,
					A2(
						elm$file$File$Select$file,
						_List_fromArray(
							['application/json']),
						author$project$Devs$TypeObject$DBLoaded));
			case 24:
				var file = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						elm$core$Task$perform,
						author$project$Devs$TypeObject$DBDecode,
						elm$file$File$toString(file)));
			default:
				var content = msg.a;
				var _n7 = A2(elm$json$Json$Decode$decodeString, author$project$Devs$DatabaseDecode$dbDecoder, content);
				if (!_n7.$) {
					var obj = _n7.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{aW: obj.aW, bb: obj.bb}),
						elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									author$project$Devs$Ports$pushDataToStore(
									{aW: obj.aW, bb: obj.bb, bh: false}),
									author$project$Devs$Utils$setPublicHolidays(
									_Utils_update(
										model,
										{aW: obj.aW, bb: obj.bb}))
								])));
				} else {
					var error = _n7.a;
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
		}
	});
var author$project$Devs$Objects$getEmptyConfig = {aj: 'NI', be: 'https://ipty.de/feiertag/api.php?do=getFeiertage&loc=[fedState]&outformat=Y-m-d&jahr=[year]', bi: false, bk: 30, bl: 5, bs: elm$core$Maybe$Nothing, aA: _List_Nil, bt: 0};
var author$project$Devs$Objects$initialModel = {
	ac: {ao: _List_Nil, M: 2019},
	aW: author$project$Devs$Objects$getEmptyConfig,
	af: elm$core$Maybe$Nothing,
	ag: 1977,
	bb: _List_Nil,
	as: 0,
	at: 0,
	aH: false,
	aI: false,
	aJ: false,
	aN: elm$time$Time$utc,
	aO: elm$core$Maybe$Nothing,
	aP: elm$core$Maybe$Nothing
};
var author$project$HolidayPlan$init = _Utils_Tuple2(
	author$project$Devs$Objects$initialModel,
	elm$core$Platform$Cmd$batch(
		_List_fromArray(
			[
				author$project$Devs$Ports$pushDataToStore(
				{aW: author$project$Devs$Objects$initialModel.aW, bb: author$project$Devs$Objects$initialModel.bb, bh: true}),
				author$project$Devs$Ports$getDimOfElement('app')
			])));
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$null = _Json_decodeNull;
var author$project$Devs$Ports$setDataFromStore = _Platform_incomingPort(
	'setDataFromStore',
	A2(
		elm$json$Json$Decode$andThen,
		function (init) {
			return A2(
				elm$json$Json$Decode$andThen,
				function (holList) {
					return A2(
						elm$json$Json$Decode$andThen,
						function (config) {
							return elm$json$Json$Decode$succeed(
								{aW: config, bb: holList, bh: init});
						},
						A2(
							elm$json$Json$Decode$field,
							'config',
							A2(
								elm$json$Json$Decode$andThen,
								function (random) {
									return A2(
										elm$json$Json$Decode$andThen,
										function (pubHolList) {
											return A2(
												elm$json$Json$Decode$andThen,
												function (password) {
													return A2(
														elm$json$Json$Decode$andThen,
														function (maxLearningHoliday) {
															return A2(
																elm$json$Json$Decode$andThen,
																function (maxHoliday) {
																	return A2(
																		elm$json$Json$Decode$andThen,
																		function (loggedIn) {
																			return A2(
																				elm$json$Json$Decode$andThen,
																				function (holidayURL) {
																					return A2(
																						elm$json$Json$Decode$andThen,
																						function (fedState) {
																							return elm$json$Json$Decode$succeed(
																								{aj: fedState, be: holidayURL, bi: loggedIn, bk: maxHoliday, bl: maxLearningHoliday, bs: password, aA: pubHolList, bt: random});
																						},
																						A2(elm$json$Json$Decode$field, 'fedState', elm$json$Json$Decode$string));
																				},
																				A2(elm$json$Json$Decode$field, 'holidayURL', elm$json$Json$Decode$string));
																		},
																		A2(elm$json$Json$Decode$field, 'loggedIn', elm$json$Json$Decode$bool));
																},
																A2(elm$json$Json$Decode$field, 'maxHoliday', elm$json$Json$Decode$int));
														},
														A2(elm$json$Json$Decode$field, 'maxLearningHoliday', elm$json$Json$Decode$int));
												},
												A2(
													elm$json$Json$Decode$field,
													'password',
													elm$json$Json$Decode$oneOf(
														_List_fromArray(
															[
																elm$json$Json$Decode$null(elm$core$Maybe$Nothing),
																A2(elm$json$Json$Decode$map, elm$core$Maybe$Just, elm$json$Json$Decode$string)
															]))));
										},
										A2(
											elm$json$Json$Decode$field,
											'pubHolList',
											elm$json$Json$Decode$list(
												A2(
													elm$json$Json$Decode$andThen,
													function (year) {
														return A2(
															elm$json$Json$Decode$andThen,
															function (pubHolList) {
																return A2(
																	elm$json$Json$Decode$andThen,
																	function (fedState) {
																		return elm$json$Json$Decode$succeed(
																			{aj: fedState, aA: pubHolList, aS: year});
																	},
																	A2(elm$json$Json$Decode$field, 'fedState', elm$json$Json$Decode$string));
															},
															A2(
																elm$json$Json$Decode$field,
																'pubHolList',
																elm$json$Json$Decode$list(
																	A2(
																		elm$json$Json$Decode$andThen,
																		function (title) {
																			return A2(
																				elm$json$Json$Decode$andThen,
																				function (date) {
																					return elm$json$Json$Decode$succeed(
																						{aX: date, bB: title});
																				},
																				A2(elm$json$Json$Decode$field, 'date', elm$json$Json$Decode$string));
																		},
																		A2(elm$json$Json$Decode$field, 'title', elm$json$Json$Decode$string)))));
													},
													A2(elm$json$Json$Decode$field, 'year', elm$json$Json$Decode$int)))));
								},
								A2(elm$json$Json$Decode$field, 'random', elm$json$Json$Decode$int))));
				},
				A2(
					elm$json$Json$Decode$field,
					'holList',
					elm$json$Json$Decode$list(
						A2(
							elm$json$Json$Decode$andThen,
							function (uuid) {
								return A2(
									elm$json$Json$Decode$andThen,
									function (to) {
										return A2(
											elm$json$Json$Decode$andThen,
											function (holType) {
												return A2(
													elm$json$Json$Decode$andThen,
													function (from) {
														return elm$json$Json$Decode$succeed(
															{a2: from, bc: holType, bC: to, bG: uuid});
													},
													A2(
														elm$json$Json$Decode$field,
														'from',
														A2(
															elm$json$Json$Decode$andThen,
															function (year) {
																return A2(
																	elm$json$Json$Decode$andThen,
																	function (month) {
																		return A2(
																			elm$json$Json$Decode$andThen,
																			function (day) {
																				return elm$json$Json$Decode$succeed(
																					{aY: day, bn: month, aS: year});
																			},
																			A2(elm$json$Json$Decode$field, 'day', elm$json$Json$Decode$int));
																	},
																	A2(elm$json$Json$Decode$field, 'month', elm$json$Json$Decode$int));
															},
															A2(elm$json$Json$Decode$field, 'year', elm$json$Json$Decode$int))));
											},
											A2(elm$json$Json$Decode$field, 'holType', elm$json$Json$Decode$int));
									},
									A2(
										elm$json$Json$Decode$field,
										'to',
										A2(
											elm$json$Json$Decode$andThen,
											function (year) {
												return A2(
													elm$json$Json$Decode$andThen,
													function (month) {
														return A2(
															elm$json$Json$Decode$andThen,
															function (day) {
																return elm$json$Json$Decode$succeed(
																	{aY: day, bn: month, aS: year});
															},
															A2(elm$json$Json$Decode$field, 'day', elm$json$Json$Decode$int));
													},
													A2(elm$json$Json$Decode$field, 'month', elm$json$Json$Decode$int));
											},
											A2(elm$json$Json$Decode$field, 'year', elm$json$Json$Decode$int))));
							},
							A2(elm$json$Json$Decode$field, 'uuid', elm$json$Json$Decode$string)))));
		},
		A2(elm$json$Json$Decode$field, 'init', elm$json$Json$Decode$bool)));
var author$project$Devs$Ports$setDimOfElement = _Platform_incomingPort(
	'setDimOfElement',
	A2(
		elm$json$Json$Decode$andThen,
		function (width) {
			return A2(
				elm$json$Json$Decode$andThen,
				function (height) {
					return elm$json$Json$Decode$succeed(
						{a9: height, bK: width});
				},
				A2(elm$json$Json$Decode$field, 'height', elm$json$Json$Decode$int));
		},
		A2(elm$json$Json$Decode$field, 'width', elm$json$Json$Decode$int)));
var author$project$Devs$TypeObject$ReadDataFromPublish = function (a) {
	return {$: 3, a: a};
};
var author$project$Devs$TypeObject$SetDomDim = function (a) {
	return {$: 27, a: a};
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var author$project$HolidayPlan$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				author$project$Devs$Ports$setDataFromStore(author$project$Devs$TypeObject$ReadDataFromPublish),
				author$project$Devs$Ports$setDimOfElement(author$project$Devs$TypeObject$SetDomDim)
			]));
};
var author$project$Devs$Objects$Down = 1;
var author$project$Devs$Objects$Up = 0;
var author$project$Devs$TypeObject$DownloadDB = {$: 22};
var author$project$Devs$TypeObject$ImportDB = {$: 23};
var author$project$Devs$TypeObject$Logout = {$: 10};
var author$project$Devs$TypeObject$ShiftYear = function (a) {
	return {$: 4, a: a};
};
var author$project$Devs$TypeObject$ToggleConfigForm = {$: 8};
var author$project$Devs$TypeObject$ToggleHolidayForm = {$: 11};
var author$project$Devs$TypeObject$ToggleLoginForm = {$: 9};
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var elm$html$Html$input = _VirtualDom_node('input');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$Templates$Utils$getActionButton = F3(
	function (label, showBtn, event) {
		return showBtn ? A2(
			elm$html$Html$input,
			_List_fromArray(
				[
					elm$html$Html$Attributes$type_('button'),
					elm$html$Html$Attributes$class('no-print'),
					elm$html$Html$Attributes$value(label),
					elm$html$Html$Events$onClick(event)
				]),
			_List_Nil) : elm$html$Html$text('');
	});
var elm$html$Html$div = _VirtualDom_node('div');
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var author$project$Templates$Forms$getFormDiv = F3(
	function (width, subForm, event) {
		var leftSpace = ((width - 524) / 2) | 0;
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('formBG')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('formDiv'),
							A2(
							elm$html$Html$Attributes$style,
							'margin-left',
							elm$core$String$fromInt(leftSpace) + 'px')
						]),
					_List_fromArray(
						[
							subForm,
							A2(
							elm$html$Html$div,
							_List_fromArray(
								[
									elm$html$Html$Attributes$class('formDivRow')
								]),
							_List_fromArray(
								[
									A3(author$project$Templates$Utils$getActionButton, 'schließen', true, event)
								]))
						]))
				]));
	});
var author$project$Devs$TypeObject$SetPW = function (a) {
	return {$: 16, a: a};
};
var elm$html$Html$Attributes$id = elm$html$Html$Attributes$stringProperty('id');
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var author$project$Templates$Forms$getLoginRow = function (model) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('formDivRow')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('password'),
								elm$html$Html$Attributes$id('pw'),
								elm$html$Html$Events$onInput(author$project$Devs$TypeObject$SetPW)
							]),
						_List_Nil)
					]))
			]));
};
var author$project$Templates$Forms$geLoginForm = function (model) {
	return model.aJ ? A3(
		author$project$Templates$Forms$getFormDiv,
		model.at,
		author$project$Templates$Forms$getLoginRow(model),
		author$project$Devs$TypeObject$ToggleLoginForm) : elm$html$Html$text('');
};
var author$project$Devs$TypeObject$SetFedState = function (a) {
	return {$: 14, a: a};
};
var author$project$Devs$TypeObject$SetLHol = function (a) {
	return {$: 13, a: a};
};
var author$project$Devs$TypeObject$SetMaxHol = function (a) {
	return {$: 12, a: a};
};
var author$project$Devs$TypeObject$SetUrl = function (a) {
	return {$: 15, a: a};
};
var elm$html$Html$option = _VirtualDom_node('option');
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$selected = elm$html$Html$Attributes$boolProperty('selected');
var author$project$Templates$Utils$getOption = F3(
	function (key, sel, label) {
		return A2(
			elm$html$Html$option,
			_List_fromArray(
				[
					elm$html$Html$Attributes$value(key),
					elm$html$Html$Attributes$selected(
					_Utils_eq(key, sel))
				]),
			_List_fromArray(
				[
					elm$html$Html$text(label)
				]));
	});
var elm$html$Html$label = _VirtualDom_node('label');
var elm$html$Html$select = _VirtualDom_node('select');
var elm$html$Html$Attributes$for = elm$html$Html$Attributes$stringProperty('htmlFor');
var elm_community$html_extra$Html$Events$Extra$onChange = function (onChangeAction) {
	return A2(
		elm$html$Html$Events$on,
		'change',
		A2(elm$json$Json$Decode$map, onChangeAction, elm$html$Html$Events$targetValue));
};
var author$project$Templates$Forms$getConfigRow = function (model) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('formDivRow')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$label,
						_List_fromArray(
							[
								elm$html$Html$Attributes$for('maxHD')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Urlaubstage:')
							])),
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('number'),
								elm$html$Html$Attributes$id('maxHD'),
								elm$html$Html$Attributes$value(
								elm$core$String$fromInt(model.aW.bk)),
								elm$html$Html$Events$onInput(author$project$Devs$TypeObject$SetMaxHol)
							]),
						_List_Nil)
					])),
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$label,
						_List_fromArray(
							[
								elm$html$Html$Attributes$for('maxLHD')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Bildungsurlaub:')
							])),
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('number'),
								elm$html$Html$Attributes$id('maxLHD'),
								elm$html$Html$Attributes$value(
								elm$core$String$fromInt(model.aW.bl)),
								elm$html$Html$Events$onInput(author$project$Devs$TypeObject$SetLHol)
							]),
						_List_Nil)
					])),
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$label,
						_List_fromArray(
							[
								elm$html$Html$Attributes$for('pw')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Passwort:')
							])),
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('text'),
								elm$html$Html$Attributes$id('pw'),
								elm$html$Html$Attributes$value(
								A2(elm$core$Maybe$withDefault, '', model.aP)),
								elm$html$Html$Events$onInput(author$project$Devs$TypeObject$SetPW)
							]),
						_List_Nil)
					])),
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$label,
						_List_fromArray(
							[
								elm$html$Html$Attributes$for('fState')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('B-Land:')
							])),
						A2(
						elm$html$Html$select,
						_List_fromArray(
							[
								elm$html$Html$Attributes$id('fState'),
								elm_community$html_extra$Html$Events$Extra$onChange(author$project$Devs$TypeObject$SetFedState)
							]),
						_List_fromArray(
							[
								A3(author$project$Templates$Utils$getOption, 'BW', model.aW.aj, 'Baden-Würtemberg'),
								A3(author$project$Templates$Utils$getOption, 'BY', model.aW.aj, 'Bayern'),
								A3(author$project$Templates$Utils$getOption, 'BE', model.aW.aj, 'Berlin'),
								A3(author$project$Templates$Utils$getOption, 'BB', model.aW.aj, 'Brandenburg'),
								A3(author$project$Templates$Utils$getOption, 'HB', model.aW.aj, 'Bremen'),
								A3(author$project$Templates$Utils$getOption, 'HH', model.aW.aj, 'Hamburg'),
								A3(author$project$Templates$Utils$getOption, 'HE', model.aW.aj, 'Hessen'),
								A3(author$project$Templates$Utils$getOption, 'MV', model.aW.aj, 'Mecklenburg-Vorpommern'),
								A3(author$project$Templates$Utils$getOption, 'NI', model.aW.aj, 'Niedersachsen'),
								A3(author$project$Templates$Utils$getOption, 'NW', model.aW.aj, 'Nordrhein-Westfalen'),
								A3(author$project$Templates$Utils$getOption, 'RP', model.aW.aj, 'Rheinland-Pfalz'),
								A3(author$project$Templates$Utils$getOption, 'SL', model.aW.aj, 'Saarland'),
								A3(author$project$Templates$Utils$getOption, 'SN', model.aW.aj, 'Sachsen'),
								A3(author$project$Templates$Utils$getOption, 'ST', model.aW.aj, 'Sachsen-Anhalt'),
								A3(author$project$Templates$Utils$getOption, 'SH', model.aW.aj, 'Schleswig-Holstein'),
								A3(author$project$Templates$Utils$getOption, 'TH', model.aW.aj, 'Thüringen')
							]))
					])),
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$label,
						_List_fromArray(
							[
								elm$html$Html$Attributes$for('url')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('API-URL:')
							])),
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('url'),
								elm$html$Html$Attributes$id('url'),
								elm$html$Html$Attributes$value(model.aW.be),
								elm$html$Html$Events$onInput(author$project$Devs$TypeObject$SetUrl)
							]),
						_List_Nil)
					]))
			]));
};
var author$project$Templates$Forms$getConfigForm = function (model) {
	return model.aH ? A3(
		author$project$Templates$Forms$getFormDiv,
		model.at,
		author$project$Templates$Forms$getConfigRow(model),
		author$project$Devs$TypeObject$ToggleConfigForm) : elm$html$Html$text('');
};
var author$project$Devs$TypeObject$AddHoliday = {$: 20};
var author$project$Devs$TypeObject$RemoveHoliday = function (a) {
	return {$: 21, a: a};
};
var author$project$Devs$TypeObject$SetFrom = function (a) {
	return {$: 17, a: a};
};
var author$project$Devs$TypeObject$SetTo = function (a) {
	return {$: 18, a: a};
};
var author$project$Devs$TypeObject$SetType = function (a) {
	return {$: 19, a: a};
};
var author$project$Devs$Utils$dateToDisplaystring = function (d) {
	return A3(
		elm$core$String$padLeft,
		2,
		'0',
		elm$core$String$fromInt(d.aY)) + ('.' + (A3(
		elm$core$String$padLeft,
		2,
		'0',
		elm$core$String$fromInt(d.bn)) + ('.' + elm$core$String$fromInt(d.aS))));
};
var author$project$Devs$Utils$dateToString = function (date) {
	return author$project$Devs$Utils$daterecordToString(
		A2(author$project$Devs$Utils$dateToCivil, elm$time$Time$utc, date));
};
var author$project$Devs$Utils$holTypeToString = function (hType) {
	switch (hType) {
		case 0:
			return 'Krank';
		case 1:
			return 'Karenz';
		case 2:
			return 'Urlaub';
		default:
			return 'Bildungsurlaub';
	}
};
var elm$core$List$sortWith = _List_sortWith;
var elm$html$Html$table = _VirtualDom_node('table');
var elm$html$Html$td = _VirtualDom_node('td');
var elm$html$Html$tr = _VirtualDom_node('tr');
var elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'focus',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$Templates$Forms$getHolidayRow = function (model) {
	var holTypeIdx = function () {
		var _n2 = model.aO;
		if (!_n2.$) {
			var holiday = _n2.a;
			return holiday.bc;
		} else {
			return author$project$Devs$Utils$holTypeToInt(author$project$Devs$Objects$getDefaultHoliday);
		}
	}();
	var holList = A2(
		elm$core$List$filter,
		function (a) {
			return _Utils_eq(a.a2.aS, model.ag) || _Utils_eq(a.bC.aS, model.ag);
		},
		A2(
			elm$core$List$sortWith,
			function (a) {
				return function (b) {
					return A2(
						elm$core$Basics$compare,
						author$project$Devs$Utils$dateToString(a.a2),
						author$project$Devs$Utils$dateToString(b.a2));
				};
			},
			model.bb));
	var holFrom = function () {
		var _n1 = model.aO;
		if (!_n1.$) {
			var holiday = _n1.a;
			return author$project$Devs$Utils$dateToString(holiday.a2);
		} else {
			return '';
		}
	}();
	var holTo = function () {
		var _n0 = model.aO;
		if (!_n0.$) {
			var holiday = _n0.a;
			return author$project$Devs$Utils$dateToString(holiday.bC);
		} else {
			return holFrom;
		}
	}();
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('formDivRow')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('date'),
								elm$html$Html$Attributes$id('from'),
								elm$html$Html$Attributes$value(holFrom),
								elm$html$Html$Events$onInput(author$project$Devs$TypeObject$SetFrom)
							]),
						_List_Nil),
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('date'),
								elm$html$Html$Attributes$id('to'),
								elm$html$Html$Attributes$value(holTo),
								elm$html$Html$Events$onInput(author$project$Devs$TypeObject$SetTo),
								elm$html$Html$Events$onFocus(
								author$project$Devs$TypeObject$SetTo(holFrom))
							]),
						_List_Nil),
						A2(
						elm$html$Html$select,
						_List_fromArray(
							[
								elm$html$Html$Attributes$id('type'),
								elm_community$html_extra$Html$Events$Extra$onChange(author$project$Devs$TypeObject$SetType)
							]),
						A2(
							elm$core$List$map,
							function (i) {
								return A3(
									author$project$Templates$Utils$getOption,
									elm$core$String$fromInt(i),
									elm$core$String$fromInt(holTypeIdx),
									author$project$Devs$Utils$holTypeToString(
										author$project$Devs$Utils$intToHoltype(i)));
							},
							A2(elm$core$List$range, 0, 3))),
						A3(author$project$Templates$Utils$getActionButton, '+', true, author$project$Devs$TypeObject$AddHoliday)
					])),
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$table,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('holTable')
							]),
						A2(
							elm$core$List$map,
							function (h) {
								return A2(
									elm$html$Html$tr,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											elm$html$Html$td,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text(
													author$project$Devs$Utils$holTypeToString(
														author$project$Devs$Utils$intToHoltype(h.bc)))
												])),
											A2(
											elm$html$Html$td,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text(
													author$project$Devs$Utils$dateToDisplaystring(h.a2))
												])),
											A2(
											elm$html$Html$td,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text(
													author$project$Devs$Utils$dateToDisplaystring(h.bC))
												])),
											A2(
											elm$html$Html$td,
											_List_Nil,
											_List_fromArray(
												[
													A3(
													author$project$Templates$Utils$getActionButton,
													'-',
													true,
													author$project$Devs$TypeObject$RemoveHoliday(h.bG))
												]))
										]));
							},
							holList))
					]))
			]));
};
var author$project$Templates$Forms$getHolidayForm = function (model) {
	return model.aI ? A3(
		author$project$Templates$Forms$getFormDiv,
		model.at,
		author$project$Templates$Forms$getHolidayRow(model),
		author$project$Devs$TypeObject$ToggleHolidayForm) : elm$html$Html$text('');
};
var elm$html$Html$Attributes$title = elm$html$Html$Attributes$stringProperty('title');
var author$project$Templates$Month$getWeekday = F2(
	function (mNr, wd) {
		var holidayName = function () {
			var _n0 = wd.aY.bd;
			if (!_n0.$) {
				var name = _n0.a;
				return name;
			} else {
				var _n1 = wd.aY.bc;
				if (!_n1.$) {
					var holType = _n1.a;
					return author$project$Devs$Utils$holTypeToString(
						author$project$Devs$Utils$intToHoltype(holType));
				} else {
					return '';
				}
			}
		}();
		var dayClass = (!_Utils_eq(mNr + 1, wd.aY.aX.bn)) ? 'otherMonth' : (((wd.M === 'Sa') || (wd.M === 'So')) ? 'weekend' : ((!_Utils_eq(wd.aY.bd, elm$core$Maybe$Nothing)) ? 'publicHoliday' : ((_Utils_eq(
			wd.aY.bc,
			elm$core$Maybe$Just(
				author$project$Devs$Utils$holTypeToInt(3))) || _Utils_eq(
			wd.aY.bc,
			elm$core$Maybe$Just(
				author$project$Devs$Utils$holTypeToInt(2)))) ? 'holiday' : ((_Utils_eq(
			wd.aY.bc,
			elm$core$Maybe$Just(
				author$project$Devs$Utils$holTypeToInt(0))) || _Utils_eq(
			wd.aY.bc,
			elm$core$Maybe$Just(
				author$project$Devs$Utils$holTypeToInt(1)))) ? 'illness' : ''))));
		return A2(
			elm$html$Html$td,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class(dayClass),
					elm$html$Html$Attributes$title(holidayName)
				]),
			_List_fromArray(
				[
					elm$html$Html$text(
					A3(
						elm$core$String$padLeft,
						2,
						'0',
						elm$core$String$fromInt(wd.aY.aX.aY)))
				]));
	});
var elm$html$Html$th = _VirtualDom_node('th');
var author$project$Templates$Month$getWeek = F2(
	function (mNr, week) {
		return A2(
			elm$html$Html$tr,
			_List_Nil,
			A2(
				elm$core$List$append,
				_List_fromArray(
					[
						A2(
						elm$html$Html$th,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text(
								elm$core$String$fromInt(week.aq))
							]))
					]),
				A2(
					elm$core$List$map,
					author$project$Templates$Month$getWeekday(mNr),
					week.bI)));
	});
var author$project$Templates$Month$getWeekdayHeader = function (wd) {
	return A2(
		elm$html$Html$th,
		_List_Nil,
		_List_fromArray(
			[
				elm$html$Html$text(wd.M)
			]));
};
var elm$html$Html$tbody = _VirtualDom_node('tbody');
var elm$html$Html$thead = _VirtualDom_node('thead');
var elm$html$Html$Attributes$colspan = function (n) {
	return A2(
		_VirtualDom_attribute,
		'colspan',
		elm$core$String$fromInt(n));
};
var author$project$Templates$Month$getMonth = function (month) {
	var week = function () {
		var _n0 = elm$core$List$head(month.bJ);
		if (!_n0.$) {
			var w = _n0.a;
			return w;
		} else {
			return {aq: 0, bI: _List_Nil};
		}
	}();
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('monthDiv')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$table,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$thead,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								elm$html$Html$tr,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										elm$html$Html$th,
										_List_fromArray(
											[
												elm$html$Html$Attributes$colspan(8)
											]),
										_List_fromArray(
											[
												elm$html$Html$text(month.M)
											]))
									])),
								A2(
								elm$html$Html$tr,
								_List_Nil,
								A2(
									elm$core$List$append,
									_List_fromArray(
										[
											A2(
											elm$html$Html$th,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Nr.')
												]))
										]),
									A2(elm$core$List$map, author$project$Templates$Month$getWeekdayHeader, week.bI)))
							])),
						A2(
						elm$html$Html$tbody,
						_List_Nil,
						A2(
							elm$core$List$map,
							author$project$Templates$Month$getWeek(month.aq),
							month.bJ))
					]))
			]));
};
var author$project$Devs$Utils$isDayForCount = F2(
	function (d, ht) {
		var zone = elm$time$Time$utc;
		var date = AdrianRibao$elm_derberos_date$Derberos$Date$Core$civilToPosix(
			A2(author$project$Devs$Utils$dateToCivil, zone, d.aX));
		var wd = A2(AdrianRibao$elm_derberos_date$Derberos$Date$Utils$getWeekday, zone, date);
		return _Utils_eq(
			d.bc,
			elm$core$Maybe$Just(
				author$project$Devs$Utils$holTypeToInt(ht))) && (_Utils_eq(d.bd, elm$core$Maybe$Nothing) && ((wd !== 5) && (wd !== 6)));
	});
var elm$core$List$sum = function (numbers) {
	return A3(elm$core$List$foldl, elm$core$Basics$add, 0, numbers);
};
var author$project$Devs$Utils$getDaysForSummery = F2(
	function (holType, yearCal) {
		return elm$core$List$sum(
			A2(
				elm$core$List$map,
				function (m) {
					return elm$core$List$length(
						A2(
							elm$core$List$filter,
							function (d) {
								return A2(author$project$Devs$Utils$isDayForCount, d, holType);
							},
							m.aZ));
				},
				yearCal.ao));
	});
var author$project$Templates$Month$getSummeryDiv = function (model) {
	var sumLhol = A2(author$project$Devs$Utils$getDaysForSummery, 3, model.ac);
	var sumKar = A2(author$project$Devs$Utils$getDaysForSummery, 1, model.ac);
	var sumIll = A2(author$project$Devs$Utils$getDaysForSummery, 0, model.ac);
	var sumHol = A2(author$project$Devs$Utils$getDaysForSummery, 2, model.ac);
	var lHolRow = (sumLhol > 0) ? A2(
		elm$html$Html$tr,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$th,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(
						author$project$Devs$Utils$holTypeToString(
							author$project$Devs$Utils$intToHoltype(3)))
					])),
				A2(
				elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(
						elm$core$String$fromInt(model.aW.bl))
					])),
				A2(
				elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(
						elm$core$String$fromInt(sumLhol))
					])),
				A2(
				elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(
						elm$core$String$fromInt(model.aW.bl - sumLhol))
					]))
			])) : elm$html$Html$text('');
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('summeryDiv')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$table,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('')
									])),
								A2(
								elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('Soll')
									])),
								A2(
								elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('Ist')
									])),
								A2(
								elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('Rest')
									]))
							])),
						A2(
						elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(
										author$project$Devs$Utils$holTypeToString(
											author$project$Devs$Utils$intToHoltype(2)))
									])),
								A2(
								elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(
										elm$core$String$fromInt(model.aW.bk))
									])),
								A2(
								elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(
										elm$core$String$fromInt(sumHol))
									])),
								A2(
								elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(
										elm$core$String$fromInt(model.aW.bk - sumHol))
									]))
							])),
						lHolRow,
						A2(
						elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(
										author$project$Devs$Utils$holTypeToString(
											author$project$Devs$Utils$intToHoltype(0)))
									])),
								A2(
								elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('')
									])),
								A2(
								elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(
										elm$core$String$fromInt(sumIll))
									])),
								A2(
								elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('')
									]))
							])),
						A2(
						elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(
										author$project$Devs$Utils$holTypeToString(
											author$project$Devs$Utils$intToHoltype(1)))
									])),
								A2(
								elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('')
									])),
								A2(
								elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(
										elm$core$String$fromInt(sumKar))
									])),
								A2(
								elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('')
									]))
							]))
					]))
			]));
};
var author$project$Templates$Utils$showOptionButton = function (model) {
	return _Utils_eq(model.aW.bs, elm$core$Maybe$Nothing) ? true : model.aW.bi;
};
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$html$Html$h1 = _VirtualDom_node('h1');
var elm$html$Html$span = _VirtualDom_node('span');
var author$project$HolidayPlan$view = function (model) {
	return A2(
		elm$html$Html$div,
		_List_Nil,
		elm$core$List$concat(
			_List_fromArray(
				[
					_List_fromArray(
					[
						author$project$Templates$Forms$getConfigForm(model),
						author$project$Templates$Forms$getHolidayForm(model),
						author$project$Templates$Forms$geLoginForm(model),
						A2(
						elm$html$Html$h1,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text('Kalender des Jahres '),
								A3(
								author$project$Templates$Utils$getActionButton,
								'<',
								true,
								author$project$Devs$TypeObject$ShiftYear(1)),
								elm$html$Html$text(
								' ' + (elm$core$String$fromInt(model.ac.M) + ' ')),
								A3(
								author$project$Templates$Utils$getActionButton,
								'>',
								true,
								author$project$Devs$TypeObject$ShiftYear(0))
							])),
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								elm$html$Html$span,
								_List_fromArray(
									[
										A2(elm$html$Html$Attributes$style, 'margin-left', '5pt')
									]),
								_List_Nil),
								A3(
								author$project$Templates$Utils$getActionButton,
								'Konfiguration',
								author$project$Templates$Utils$showOptionButton(model),
								author$project$Devs$TypeObject$ToggleConfigForm),
								A2(
								elm$html$Html$span,
								_List_fromArray(
									[
										A2(elm$html$Html$Attributes$style, 'margin-left', '5pt')
									]),
								_List_Nil),
								A3(
								author$project$Templates$Utils$getActionButton,
								'Abwesenheiten',
								author$project$Templates$Utils$showOptionButton(model),
								author$project$Devs$TypeObject$ToggleHolidayForm),
								A2(
								elm$html$Html$span,
								_List_fromArray(
									[
										A2(elm$html$Html$Attributes$style, 'margin-left', '5pt')
									]),
								_List_Nil),
								A3(
								author$project$Templates$Utils$getActionButton,
								'Export',
								author$project$Templates$Utils$showOptionButton(model),
								author$project$Devs$TypeObject$DownloadDB),
								A2(
								elm$html$Html$span,
								_List_fromArray(
									[
										A2(elm$html$Html$Attributes$style, 'margin-left', '5pt')
									]),
								_List_Nil),
								A3(author$project$Templates$Utils$getActionButton, 'Import', true, author$project$Devs$TypeObject$ImportDB),
								A2(
								elm$html$Html$span,
								_List_fromArray(
									[
										A2(elm$html$Html$Attributes$style, 'margin-left', '5pt')
									]),
								_List_Nil),
								A3(
								author$project$Templates$Utils$getActionButton,
								'Login',
								(!_Utils_eq(model.aW.bs, elm$core$Maybe$Nothing)) && (!model.aW.bi),
								author$project$Devs$TypeObject$ToggleLoginForm),
								A3(
								author$project$Templates$Utils$getActionButton,
								'Logout',
								(!_Utils_eq(model.aW.bs, elm$core$Maybe$Nothing)) && model.aW.bi,
								author$project$Devs$TypeObject$Logout)
							]))
					]),
					A2(elm$core$List$map, author$project$Templates$Month$getMonth, model.ac.ao),
					_List_fromArray(
					[
						author$project$Templates$Month$getSummeryDiv(model)
					])
				])));
};
var elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var elm$browser$Browser$Dom$NotFound = elm$core$Basics$identity;
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = 0;
var elm$url$Url$Https = 1;
var elm$core$String$indexes = _String_indexes;
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {ak: fragment, al: host, au: path, aw: port_, az: protocol, aB: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 1) {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		0,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		1,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$element = _Browser_element;
var author$project$HolidayPlan$main = elm$browser$Browser$element(
	{
		bh: function (_n0) {
			return author$project$HolidayPlan$init;
		},
		bz: author$project$HolidayPlan$subscriptions,
		bE: author$project$Devs$Update$update,
		bH: author$project$HolidayPlan$view
	});
_Platform_export({'HolidayPlan':{'init':author$project$HolidayPlan$main(
	elm$json$Json$Decode$succeed(0))(0)}});}(this));