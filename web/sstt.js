(function(a){typeof
globalThis!=="object"&&(this?b():(a.defineProperty(a.prototype,"_T_",{configurable:true,get:b}),_T_));function
b(){var
b=this||self;b.globalThis=b;delete
a.prototype._T_}}(Object));($=>async a=>{"use strict";const{link:k,src:Y,generated:M,disable_effects:J}=a,h=globalThis.process?.versions?.node,U={cos:Math.cos,sin:Math.sin,tan:Math.tan,acos:Math.acos,asin:Math.asin,atan:Math.atan,cosh:Math.cosh,sinh:Math.sinh,tanh:Math.tanh,acosh:Math.acosh,asinh:Math.asinh,atanh:Math.atanh,cbrt:Math.cbrt,exp:Math.exp,expm1:Math.expm1,log:Math.log,log1p:Math.log1p,log2:Math.log2,log10:Math.log10,atan2:Math.atan2,hypot:Math.hypot,pow:Math.pow,fmod:(a,b)=>a%b},z=[Float32Array,Float64Array,Int8Array,Uint8Array,Int16Array,Uint16Array,Int32Array,Int32Array,Int32Array,Int32Array,Float32Array,Float64Array,Uint8Array,Uint16Array,Uint8ClampedArray],f=h&&require("node:fs"),b=f?.constants,A=f?[b.R_OK,b.W_OK,b.X_OK,b.F_OK]:[],V=f?[b.O_RDONLY,b.O_WRONLY,b.O_RDWR,b.O_APPEND,b.O_CREAT,b.O_TRUNC,b.O_EXCL,b.O_NONBLOCK,b.O_NOCTTY,b.O_DSYNC,b.O_SYNC]:[];var
e={map:new
WeakMap(),set:new
Set(),finalization:new
FinalizationRegistry(a=>e.set.delete(a))};function
X(a){const
b=new
WeakRef(a);e.map.set(a,b);e.set.add(b);e.finalization.register(a,b,a)}function
Z(a){const
b=e.map.get(a);if(b){e.map.delete(a);e.set.delete(b);e.finalization.unregister(a)}}function
I(){return[...e.set].map(a=>a.deref()).filter(a=>a)}var
y;function
T(a){return WebAssembly?.Suspending?new
WebAssembly.Suspending(a):a}function
v(a){return!J&&WebAssembly?.promising&&a?WebAssembly.promising(a):a}const
o=new
TextDecoder("utf-8",{ignoreBOM:1}),K=new
TextEncoder();function
N(a,b){b=Math.imul(b,0xcc9e2d51|0);b=b<<15|b>>>17;b=Math.imul(b,0x1b873593);a^=b;a=a<<13|a>>>19;return(a+(a<<2)|0)+(0xe6546b64|0)|0}function
O(a,b){for(var
c=0;c<b.length;c++)a=N(a,b.charCodeAt(c));return a^b.length}function
u(a){if(h&&globalThis.process.env[a]!==undefined)return globalThis.process.env[a];return globalThis.jsoo_env?.[a]}let
l=0;for(const
a
of
u("OCAMLRUNPARAM")?.split(",")||[]){if(a==="b")l=1;if(a.startsWith("b="))l=+a.slice(2)?1:0}function
n(a,b){var
c;if(a.isFile())c=0;else if(a.isDirectory())c=1;else if(a.isCharacterDevice())c=2;else if(a.isBlockDevice())c=3;else if(a.isSymbolicLink())c=4;else if(a.isFIFO())c=5;else if(a.isSocket())c=6;return E(b,a.dev,a.ino|0,c,a.mode,a.nlink,a.uid,a.gid,a.rdev,BigInt(a.size),a.atimeMs/1000,a.mtimeMs/1000,a.ctimeMs/1000)}const
w=h&&globalThis.process.platform==="win32",d=Function.prototype.call,c=DataView.prototype,B={jstag:WebAssembly.JSTag||new
WebAssembly.Tag({parameters:["externref"],results:[]}),identity:a=>a,from_bool:a=>!!a,get:(a,b)=>a[b],set:(a,b,c)=>a[b]=c,delete:(a,b)=>delete
a[b],instanceof:(a,b)=>a
instanceof
b,typeof:a=>typeof
a,equals:(a,b)=>a==b,strict_equals:(a,b)=>a===b,fun_call:(a,b,c)=>a.apply(b,c),meth_call:(a,b,c)=>a[b].apply(a,c),new_array:a=>new
Array(a),new_obj:()=>({}),new:(a,b)=>new
a(...b),global_this:globalThis,iter_props:(a,b)=>{for(var
c
in
a)if(Object.hasOwn(a,c))b(c)},array_length:a=>a.length,array_get:(a,b)=>a[b],array_set:(a,b,c)=>a[b]=c,read_string:a=>o.decode(new
Uint8Array(j,0,a)),read_string_stream:(a,b)=>o.decode(new
Uint8Array(j,0,a),{stream:b}),append_string:(a,b)=>a+b,write_string:a=>{var
c=0,b=a.length;for(;;){const{read:d,written:e}=K.encodeInto(a.slice(c),W);b-=d;if(!b)return e;G(e);c+=d}},ta_create:(a,b)=>new
z[a](b),ta_normalize:a=>a
instanceof
Uint32Array?new
Int32Array(a.buffer,a.byteOffset,a.length):a,ta_kind:b=>z.findIndex(a=>b
instanceof
a),ta_length:a=>a.length,ta_get_i32:(a,b)=>a[b],ta_fill:(a,b)=>a.fill(b),ta_blit:(a,b)=>b.set(a),ta_subarray:(a,b,c)=>a.subarray(b,c),ta_set:(a,b,c)=>a.set(b,c),ta_new:a=>new
Uint8Array(a),ta_copy:(a,b,c,d)=>a.copyWithin(b,c,d),ta_bytes:a=>new
Uint8Array(a.buffer,a.byteOffset,a.length*a.BYTES_PER_ELEMENT),ta_blit_from_bytes:(a,b,c,d,e)=>{for(let
f=0;f<e;f++)c[d+f]=C(a,b+f)},ta_blit_to_bytes:(a,b,c,d,e)=>{for(let
f=0;f<e;f++)D(c,d+f,a[b+f])},dv_make:a=>new
DataView(a.buffer,a.byteOffset,a.byteLength),dv_get_f64:d.bind(c.getFloat64),dv_get_f32:d.bind(c.getFloat32),dv_get_i64:d.bind(c.getBigInt64),dv_get_i32:d.bind(c.getInt32),dv_get_i16:d.bind(c.getInt16),dv_get_ui16:d.bind(c.getUint16),dv_get_i8:d.bind(c.getInt8),dv_get_ui8:d.bind(c.getUint8),dv_set_f64:d.bind(c.setFloat64),dv_set_f32:d.bind(c.setFloat32),dv_set_i64:d.bind(c.setBigInt64),dv_set_i32:d.bind(c.setInt32),dv_set_i16:d.bind(c.setInt16),dv_set_i8:d.bind(c.setInt8),littleEndian:new
Uint8Array(new
Uint32Array([1]).buffer)[0],wrap_callback:b=>function(...a){if(a.length===0)a=[undefined];return g(b,a.length,a,1)},wrap_callback_args:b=>function(...a){return g(b,1,[a],0)},wrap_callback_strict:(c,b)=>function(...a){a.length=c;return g(b,c,a,0)},wrap_callback_unsafe:b=>function(...a){return g(b,a.length,a,2)},wrap_meth_callback:b=>function(...a){a.unshift(this);return g(b,a.length,a,1)},wrap_meth_callback_args:b=>function(...a){return g(b,2,[this,a],0)},wrap_meth_callback_strict:(c,b)=>function(...a){a.length=c;a.unshift(this);return g(b,a.length,a,0)},wrap_meth_callback_unsafe:b=>function(...a){a.unshift(this);return g(b,a.length,a,2)},wrap_fun_arguments:b=>function(...a){return b(a)},format_float:(a,b,c,d)=>{function
j(a,b){if(Math.abs(a)<1.0)return a.toFixed(b);else{var
c=Number.parseInt(a.toString().split("+")[1]);if(c>20){c-=20;a/=Math.pow(10,c);a+=new
Array(c+1).join("0");if(b>0)a=a+"."+new
Array(b+1).join("0");return a}else
return a.toFixed(b)}}switch(b){case
0:var
e=d.toExponential(a),f=e.length;if(e.charAt(f-3)==="e")e=e.slice(0,f-1)+"0"+e.slice(f-1);break;case
1:e=j(d,a);break;case
2:a=a?a:1;e=d.toExponential(a-1);var
i=e.indexOf("e"),h=+e.slice(i+1);if(h<-4||d>=1e21||d.toFixed(0).length>a){var
f=i-1;while(e.charAt(f)==="0")f--;if(e.charAt(f)===".")f--;e=e.slice(0,f+1)+e.slice(i);f=e.length;if(e.charAt(f-3)==="e")e=e.slice(0,f-1)+"0"+e.slice(f-1);break}else{var
g=a;if(h<0){g-=h+1;e=d.toFixed(g)}else
while(e=d.toFixed(g),e.length>a+1)g--;if(g){var
f=e.length-1;while(e.charAt(f)==="0")f--;if(e.charAt(f)===".")f--;e=e.slice(0,f+1)}}break}return c?" "+e:e},gettimeofday:()=>new
Date().getTime()/1000,times:()=>{if(globalThis.process?.cpuUsage){var
a=globalThis.process.cpuUsage();return q(a.user/1e6,a.system/1e6)}else{var
a=performance.now()/1000;return q(a,0)}},gmtime:a=>{var
b=new
Date(a*1000),c=b.getTime(),e=new
Date(Date.UTC(b.getUTCFullYear(),0,1)).getTime(),d=Math.floor((c-e)/86400000);return r(b.getUTCSeconds(),b.getUTCMinutes(),b.getUTCHours(),b.getUTCDate(),b.getUTCMonth(),b.getUTCFullYear()-1900,b.getUTCDay(),d,false)},localtime:a=>{var
b=new
Date(a*1000),c=b.getTime(),f=new
Date(b.getFullYear(),0,1).getTime(),d=Math.floor((c-f)/86400000),e=new
Date(b.getFullYear(),0,1),g=new
Date(b.getFullYear(),6,1),h=Math.max(e.getTimezoneOffset(),g.getTimezoneOffset());return r(b.getSeconds(),b.getMinutes(),b.getHours(),b.getDate(),b.getMonth(),b.getFullYear()-1900,b.getDay(),d,b.getTimezoneOffset()<h)},mktime:(a,b,c,d,e,f)=>new
Date(a,b,c,d,e,f).getTime(),random_seed:()=>crypto.getRandomValues(new
Int32Array(12)),access:(a,d)=>f.accessSync(a,A.reduce((a,b,c)=>d&1<<c?a|b:a,0)),open:(a,d,c)=>f.openSync(a,V.reduce((a,b,c)=>d&1<<c?a|b:a,0),c),close:a=>f.closeSync(a),write:(a,b,c,d,e)=>f?f.writeSync(a,b,c,d,e===null?e:Number(e)):(console[a===2?"error":"log"](typeof
b==="string"?b:o.decode(b.slice(c,c+d))),d),read:(a,b,c,d,e)=>f.readSync(a,b,c,d,e),fsync:a=>f.fsyncSync(a),file_size:a=>f.fstatSync(a,{bigint:true}).size,register_channel:X,unregister_channel:Z,channel_list:I,exit:a=>h&&globalThis.process.exit(a),argv:()=>h?globalThis.process.argv.slice(1):["a.out"],on_windows:+w,getenv:u,backtrace_status:()=>l,record_backtrace:a=>l=a,system:a=>{var
b=require("node:child_process").spawnSync(a,{shell:true,stdio:"inherit"});if(b.error)throw b.error;return b.signal?255:b.status},isatty:a=>+require("node:tty").isatty(a),time:()=>performance.now(),getcwd:()=>h?globalThis.process.cwd():"/static",chdir:a=>globalThis.process.chdir(a),mkdir:(a,b)=>f.mkdirSync(a,b),rmdir:a=>f.rmdirSync(a),link:(a,b)=>f.linkSync(a,b),symlink:(a,b,c)=>f.symlinkSync(a,b,[null,"file","dir"][c]),readlink:a=>f.readlinkSync(a),unlink:a=>f.unlinkSync(a),read_dir:a=>f.readdirSync(a),opendir:a=>f.opendirSync(a),readdir:a=>{var
b=a.readSync()?.name;return b===undefined?null:b},closedir:a=>a.closeSync(),stat:(a,b)=>n(f.statSync(a),b),lstat:(a,b)=>n(f.lstatSync(a),b),fstat:(a,b)=>n(f.fstatSync(a),b),chmod:(a,b)=>f.chmodSync(a,b),fchmod:(a,b)=>f.fchmodSync(a,b),file_exists:a=>+f.existsSync(a),is_directory:a=>+f.lstatSync(a).isDirectory(),is_file:a=>+f.lstatSync(a).isFile(),utimes:(a,b,c)=>f.utimesSync(a,b,c),truncate:(a,b)=>f.truncateSync(a,b),ftruncate:(a,b)=>f.ftruncateSync(a,b),rename:(a,b)=>{var
c;if(w&&(c=f.statSync(b,{throwIfNoEntry:false}))&&f.statSync(a,{throwIfNoEntry:false})?.isDirectory())if(c.isDirectory()){if(!b.startsWith(a))try{f.rmdirSync(b)}catch{}}else{var
d=new
Error(`ENOTDIR: not a directory, rename '${a}' -> '${b}'`);throw Object.assign(d,{errno:-20,code:"ENOTDIR",syscall:"rename",path:b})}f.renameSync(a,b)},tmpdir:()=>require("node:os").tmpdir(),start_fiber:a=>y(a),suspend_fiber:T((c,b)=>new
Promise(a=>c(a,b))),resume_fiber:(a,b)=>a(b),weak_new:a=>new
WeakRef(a),weak_deref:a=>{var
b=a.deref();return b===undefined?null:b},weak_map_new:()=>new
WeakMap(),map_new:()=>new
Map(),map_get:(a,b)=>{var
c=a.get(b);return c===undefined?null:c},map_set:(a,b,c)=>a.set(b,c),map_delete:(a,b)=>a.delete(b),hash_string:O,log:a=>console.log(a)},p={test:a=>+(typeof
a==="string"),compare:(a,b)=>a<b?-1:+(a>b),decodeStringFromUTF8Array:()=>"",encodeStringToUTF8Array:()=>0,fromCharCodeArray:()=>""},i=Object.assign({Math:U,bindings:B,js:$,"wasm:js-string":p,"wasm:text-decoder":p,"wasm:text-encoder":p,str:new
globalThis.Proxy({},{get(a,b){return b}}),env:{}},M),x={builtins:["js-string","text-decoder","text-encoder"],importedStringConstants:"str"};function
S(a){const
b=require("node:path"),c=b.join(b.dirname(require.main.filename),a);return require("node:fs/promises").readFile(c)}const
t=globalThis?.document?.currentScript?.src;function
L(a){const
b=t?new
URL(a,t):a;return fetch(b)}const
R=h?S:L;async function
Q(a){return h?WebAssembly.instantiate(await
a,i,x):WebAssembly.instantiateStreaming(a,i,x)}async function
P(){i.OCaml={};const
c=[];async function
b(a,b){const
f=a[1].constructor!==Array;async function
e(){const
d=R(Y+"/"+a[0]+".wasm");await
Promise.all(f?c:a[1].map(a=>c[a]));const
e=await
Q(d);Object.assign(b?i.env:i.OCaml,e.instance.exports)}const
d=e();c.push(d);return d}async function
a(a){for(const
c
of
a)await
b(c)}await
b(k[0],1);if(k.length>1){await
b(k[1]);const
c=new
Array(20).fill(k.slice(2).values()).map(a);await
Promise.all(c)}return{instance:{exports:Object.assign(i.env,i.OCaml)}}}const
_=await
P();var{caml_callback:g,caml_alloc_times:q,caml_alloc_tm:r,caml_alloc_stat:E,caml_start_fiber:H,caml_handle_uncaught_exception:s,caml_buffer:F,caml_extract_bytes:G,bytes_get:C,bytes_set:D,_initialize:m}=_.instance.exports,j=F?.buffer,W=j&&new
Uint8Array(j,0,j.length);y=v(H);var
m=v(m);if(globalThis.process?.on)globalThis.process.on("uncaughtException",(a,b)=>s(a));else if(globalThis.addEventListener)globalThis.addEventListener("error",a=>a.error&&s(a.error));await
m()})(function(a){"use strict";function
i(a){return a==BigInt.asIntN(31,a)?Number(a):a}function
b(a,b){return i(BigInt(a)+BigInt(b))}function
c(a,b){return(a>b)-(a<b)}function
d(a,b,c,d){var
f=0n;for(var
g=0;g<d/4;g++){var
e=BigInt(a(b));e|=BigInt(a(b))<<8n;e|=BigInt(a(b))<<16n;e|=BigInt(a(b))<<24n;f|=e<<BigInt(32*g)}if(c)f=-f;return f}function
g(a,b){return a==b}var
h="0",f="-",o="+",m="";function
j(a,b){b=BigInt(b);var
j=10,q=0,l=0,i=0,p=0,k=m,n=" ",g=n,d=0,e=m;while(a[d]=="%")d++;for(;;d++)if(a[d]=="#")i=1;else if(a[d]==h)g=h;else if(a[d]==f)p=1;else if(a[d]==n||a[d]==o)k=a[d];else
break;if(b<0){k=f;b=-b}for(;a[d]>=h&&a[d]<="9";d++)l=10*l+
+a[d];switch(a[d]){case"i":case"d":case"u":break;case"b":j=2;if(i)e="0b";break;case"o":j=8;if(i)e="0o";break;case"x":j=16;if(i)e="0x";break;case"X":j=16;if(i)e="0X";q=1;break;default:return-1}if(p)g=n;var
c=b.toString(j);if(q===1)c=c.toUpperCase();var
s=c.length;if(g==n)if(p){c=k+e+c;for(;c.length<l;)c=c+g}else{c=k+e+c;for(;c.length<l;)c=g+c}else{var
r=k+e;for(;c.length+r.length<l;)c=g+c;c=r+c}return c}var
e=32n;function
k(a,b){b=BigInt(b);var
f=b<0;if(f)b=-b;var
d=0,c=0;while(b){d++;c=a(c,Number(BigInt.asIntN(32,b)));b>>=e}if(d&1)c=a(c,0);if(f)c++;return c}function
l(a,b){if(a==0){a=10;var
c=0,l=1;if(b[c]==f){l=-1;c++}else if(b[c]==o)c++;if(b[c]==h){c++;if(b.length==c)return 0;else{var
e=b[c];if(e=="o"||e=="O")a=8;else if(e=="x"||e=="X")a=16;else if(e=="b"||e=="B")a=2;if(a!=10){b=b.substring(c+1);if(l==-1)b=f+b}}}}function
p(a){if(a>=48&&a<=57)return a-48;if(a>=97&&a<=102)return a-97+10;if(a>=65&&a<=70)return a-65+10;return 1000}var
k=false,d=0;if(b[d]==o)b=b.substring(1);else if(b[d]==f){k=true;d++}if(b[d]=="_")return null;b=b.replace(/_/g,m);if(b==f||b==m)b=h;var
g=0n,n=BigInt(a);for(;d<b.length;d++){var
j=p(b.charCodeAt(d));if(j>=a)return null;g=g*n+BigInt(j)}if(k)g=-g;return i(g)}function
n(a){return a>=0}function
p(a,b,c){if(c<0)c=-c;do{var
d=Number(BigInt.asIntN(32,c));a(b,d);a(b,d>>>8);a(b,d>>>16);a(b,d>>>24);c>>=e}while(c)}function
q(a){if(a<0)a=-a;var
b=0,c=1n;while(c<=a){b+=1;c<<=e}return b}function
r(a,b){return i(BigInt(a)-BigInt(b))}return{wasm_z_sub:r,wasm_z_size:q,wasm_z_serialize:p,wasm_z_positive:n,wasm_z_of_js_string_base:l,wasm_z_hash:k,wasm_z_format:j,wasm_z_equal:g,wasm_z_deserialize:d,wasm_z_compare:c,wasm_z_add:b}}(globalThis))({"link":[["code-9d3477cb25b8d3929053",0]],"generated":(a=>{var
c=a,b=a?.module?.export||a;return{"fragments":{"get_Array":a=>a.Array,"get_Date":a=>a.Date,"get_Error":a=>a.Error,"get_JSON":a=>a.JSON,"get_Math":a=>a.Math,"get_Object":a=>a.Object,"get_RegExp":a=>a.RegExp,"get_String":a=>a.String,"get_length":a=>a.length,"js_expr_12c48ca8":()=>a,"js_expr_21711c2a":()=>b,"js_expr_26f07992":()=>null,"meth_call_0_toString":a=>a.toString(),"obj_0":(a,b,c,d)=>({send:a,commit:b,version:c,compiler:d}),"set_sstt":(a,b)=>a.sstt=b}}})(globalThis),"src":"wasm.bc.wasm.assets"});
