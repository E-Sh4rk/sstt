  $ cduce --verbose --compile web_site.cd
  val outdir : [  ]
  val input : [ '../../../common/site.xml' ]
  val load_include : Latin1 -> [ Any* ]
  val extra_head : [ H_script* ]
  val main_page : Page
  val footer : [ Item* ]
  val header : [ (<global_header>Content Item*)? ]
  val site : String
  val split_comma : String -> [ String* ]
  val xhighlight : String -> [ (H_i | H_strong | Char)* ]
  val highlight : String -> [ (H_i | H_strong | Char)* ]
  val split_thumbnails : String -> Namespaces
  val hwbox : [ Flow 'right' | Flow 'left' ] -> Block
  val hwbox_title : [ Flow String String 'right' | Flow String String 'left' ] -> Block
  val box : Flow -> Block
  val box_title : [ Flow String Char* ] -> Block
  val meta : Flow -> Block
  val small_box : Flow -> Block
  val link_to : Page -> H_a
  val boxes_of : Page -> [ H_ul? ]
  val display_sitemap : Tree -> H_li
  val ol : ([ H_li* ],{ style=?String }) -> [ H_ol? ]
  val ul : [ H_li* ] -> [ H_ul? ]
  val compute_sitemap : Page | External -> Tree
  val local_link : [ Tree String Char* ] -> [ Inline? ]
  val find_local_link : [ [ Tree* ] Char* ] -> Tree
  val authors : [ Author+ ] -> Flow
  val render : String -> { presenter=?[ 'no' | 'yes' ] .. } -> Flow
  val url_of_page : Page -> String
  val gen_page_seq : (String,(PageO,([ Page* ],(PageO,(Path,Tree))))) -> (PageO,PageO)
  val gen_page : (String,(PageO,(Page,(PageO,(Path,Tree))))) -> PageO
  val thumbwh : { width=?IntStr height=?IntStr .. } -> String -> String -> Inlines
  val thumbnail : [ String Char* ] -> String -> String -> Inlines
  val demo : Int -> String -> String -> String -> Flow
  val button_id : String -> String -> String -> String -> Inline
  val button : String -> String -> Inline
  $ cduce --run web_site.cdo
  <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"><html xmlns="http://www.w3.org/1999/xhtml"><head><title>CDuce: Quick reference</title><meta content="text/html; charset=UTF-8" http-equiv="Content-Type"/><link rel="stylesheet" media="screen, projection" href="css/screen.css" type="text/css"/><link rel="stylesheet" media="print" href="css/print.css" type="text/css"/><link rel="stylesheet" media="screen, projection" href="css/screen.css" type="text/css"/><link rel="stylesheet" href="cduce.css" type="text/css"/><script type="text/javascript">var _gaq = _gaq || [];
     	   _gaq.push(['_setAccount', 'UA-15579826-1']);
    	   _gaq.push(['_trackPageview']);
    	   (function() {
      	     var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
      	     ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
      	     var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
    	   })();</script></head><body><div class="container"><div class="span-24 last" id="header"><div class="span-24 last" id="title"><h1>Quick reference</h1></div><div class="span-24 last meta" id="global_bar"><p/></div></div><div class="span-20" id="main"><div class="span-20" id="box"><h2><a name="id">Identifiers</a></h2><ul><li> Type and Pattern identifiers: words formed by of Unicode letters and 
  the underscore &quot;_&quot; character,  starting by an uppercase letter. </li><li> value identifiers: words formed by of Unicode letters and the underscore &quot;
  _&quot; character,  starting by a lowercase letter or underscore.</li></ul></div><div class="span-20" id="box"><h2><a name="scalars">Scalars</a></h2><ul><li>Large integers: 
     <ul><li>Values: <b><tt>0,1,2,3,...</tt></b></li><li>Types: intervals <b><tt>-*--10, 20--30, 50--*, ...</tt></b>,
                  singletons <b><tt>0,1,2,3,...</tt></b></li><li>Operators: <b><tt>+,-,/,*,div,mod, int_of</tt></b></li></ul></li><li>Floats: 
     <ul><li>Values: <i>none built-in</i>. </li><li>Types: only <b><tt>Float</tt></b>. </li><li>Operators: <b><tt>float_of</tt></b> : String -&gt; Float</li></ul></li><li>Unicode characters:
     <ul><li>Values: quoted characters (<b><tt>'a'</tt></b>, <b><tt>'b'</tt></b>, 
           <b><tt>'c'</tt></b>, ...,<b><tt>'&#12354;'</tt></b>, <b><tt>'&#12356;'</tt></b>, ... , 
           <b><tt>'&#31169;'</tt></b>, ... , <b><tt>'&#8838;'</tt></b>, ...),
           codepoint-defined characters (<b><tt>'\x<i>h</i>;' '\<i>d</i>;' 
           </tt></b> where <b><tt><i>h</i></tt></b> and
           <b><tt><i>d</i></tt></b> are hexadecimal and decimal integers
           respectively), and backslash-escaped characters 
           (<b><tt>'\t'</tt></b> tab, <b><tt>'\n'</tt></b> newline, 
           <b><tt>'\r'</tt></b> return, <b><tt>'\\'</tt></b> backslash).</li><li>Types: intervals <b><tt>'a'--'z', '0'--'9'</tt></b>,
                  singletons <b><tt>'a','b','c',...</tt></b></li><li>Operators: <b><tt>char_of_int</tt></b> : Int -&gt; Char, <b><tt>int_of_char</tt></b> : Char -&gt; Int</li></ul></li><li>Symbolic atoms:
     <ul><li>Values: <b><tt>`A, `B, `a, `b, `true, `false, ...</tt></b></li><li>Types: singletons <b><tt>`A, `B, ...</tt></b></li><li>Operators: <b><tt>make_atom</tt></b> : (String,String) -&gt; Atom,
            <b><tt>split_atom</tt></b> : Atom -&gt; (String,String) </li><li>CDuce also supports </li></ul></li></ul></div><div class="span-20" id="box"><h2><a name="op">Operators, built-in functions</a></h2><ul><li>Infix:
       <br/><b><tt>@</tt></b> : concatenation of sequences
       <br/><b><tt>+,*,-,div,mod</tt></b> : Integer,Integer -&gt; Integer
       <br/><b><tt>=, &lt;&lt;, &lt;=, &gt;&gt;, &gt;= </tt></b> :
   <i>t</i>,<i>t</i> -&gt; Bool = <b><tt>`true | `false</tt></b> (any non functional type <i>t</i>)
       <br/><b><tt>||, &amp;&amp;</tt></b> : Bool,Bool -&gt; Bool
       <br/><b><tt>not</tt></b>: Bool -&gt; Bool
     </li><li>Prefix:
          <br/><b><tt>load_xml</tt></b> : Latin1 -&gt; AnyXml,
          <br/><b><tt>load_html</tt></b> : Latin1 -&gt; [ Any* ],
          <br/><b><tt>load_file</tt></b> : Latin1 -&gt; Latin1,
          <br/><b><tt>load_file_utf8</tt></b> : Latin1 -&gt; String,
          <br/><b><tt>dump_to_file</tt></b> : Latin1 -&gt; String -&gt; [],
          <br/><b><tt>dump_to_file_utf8</tt></b> : Latin1 -&gt; String -&gt; [],
          <br/><b><tt>print_xml</tt></b> : Any -&gt; Latin1,
          <br/><b><tt>print_xml_utf8</tt></b> : Any -&gt; String,
          <br/><b><tt>print</tt></b> : Latin1 -&gt; [],
          <br/><b><tt>print_utf8</tt></b> : String -&gt; [],
          <br/><b><tt>dump_xml</tt></b> : Any -&gt; [],
          <br/><b><tt>dump_xml_utf8</tt></b> : Any -&gt; [],
          <br/><b><tt>int_of</tt></b> : String -&gt; Int,
          <br/><b><tt>float_of</tt></b> : String -&gt; Float,
          <br/><b><tt>string_of</tt></b> : Any -&gt; Latin1,
          <br/><b><tt>char_of_int</tt></b> : Int -&gt; Char,
          <br/><b><tt>make_atom</tt></b> : (String,String) -&gt; Atom,
  	<br/><b><tt>split_atom</tt></b> : Atom -&gt; (String,String),
          <br/><b><tt>system</tt></b> : Latin1 -&gt; { stdout = Latin1; stderr = Latin1; 
               status = (`exited,Int) | (`stopped,Int) | (`signaled,Int)
          },
          <br/><b><tt>exit</tt></b> : 0--255 -&gt; Empty,
          <br/><b><tt>getenv</tt></b> : Latin1 -&gt; Latin1,
          <br/><b><tt>argv</tt></b> : [] -&gt; [ String* ],
          <br/><b><tt>raise</tt></b> : Any -&gt; Empty
     </li></ul></div><div class="span-20" id="box"><h2><a name="pair">Pairs</a></h2><ul><li>Expressions: <b><tt>(e1,e2)</tt></b></li><li>Types and patterns: <b><tt>(t1,t2)</tt></b></li><li>Note: tuples are right-associative pairs; e.g.: 
             <b><tt>(1,2,3)=(1,(2,3))</tt></b></li><li>When a capture variable appears on both side of a pair pattern,
       the two captured values are paired
       together (e.g. <b><tt>match (1,2,3) with (x,(_,x)) -&gt; x ==&gt;
  (1,3)</tt></b>). </li></ul></div><div class="span-20" id="box"><h2><a name="seq">Sequences</a></h2><ul><li>Expressions: <b><tt>[ 1 2 3 ]</tt></b>, 
       which is syntactic sugar for <b><tt>(1,(2,(3,`nil)))</tt></b></li><li>A sub-sequence can be escaped by !: <b><tt>[ 1 2 ![ 3 4 ] 5
  ]</tt></b> is then equal to <b><tt>[ 1 2 3 4  5 ]</tt></b> . </li><li>Types and patterns : <b><tt>[ R ]</tt></b> where <b><tt>R</tt></b> is
      a regular expression built on types and patterns:
     <ul><li>A type or a pattern is a regexp by itself, matching a single
           element of the sequence </li><li>Postfix repetition operators: <b><tt>*,+,?</tt></b>
           and the ungreedy variants (for patterns) <b><tt>*?, +?
           ,??</tt></b></li><li>Concatenation of regexps</li><li>For patterns, sequence capture variable <b><tt>x::R</tt></b></li></ul></li><li>It is possible to specify a tail, for expressions, types, and patterns;
      e.g.: <b><tt>[ x::Int*; q ]</tt></b></li><li>Map: <b><tt>map e with p1 -&gt; e1 | ... | pn -&gt; en</tt></b>. 
      Each element of e must be matched. </li><li>Transform: <b><tt>transform e with p1 -&gt; e1 | ... | pn -&gt; en</tt></b>. 
      Unmatched elements are discarded; each branch returns a sequence
      and all the resulting sequences are concatenated together. </li><li>Selection: : <b><tt>select <i>e</i> from <i>p1</i> in  <i>e1</i>  ...  <i>pn</i>
      in <i>en</i> where <i>e'</i></tt></b>. SQL-like selection with the possibility 
      of using CDuce patterns instead of variables. <b><tt><i>e1</i>  ...
      <i>en</i></tt></b> must be sequences and <b><tt><i>e'</i></tt></b> a boolean
       expression.</li><li>Operators: concatenation <b><tt>e1 @ e2 = [ !e1 !e2 ]</tt></b>,
                flattening <b><tt>flatten e = transform e with x -&gt; x</tt></b>.
  </li></ul></div><div class="span-20" id="box"><h2><a name="record">Record</a></h2><ul><li>Records literal <b><tt>{ l1 = e1; ...; ln = en }</tt></b></li><li>Types: <b><tt>{ l1 = t1; ...; ln = tn }</tt></b> (closed, no more
  fields allowed), <b><tt>{ l1 = t1; ...; ln = tn; .. }</tt></b> (open,
  any other field allowed). Optional fields: <b><tt>li =? ti</tt></b>
  instead of <b><tt>li = ti</tt></b>. Semi-colons are optional.</li><li>Record concatenation:  <b><tt>e1 + e2</tt></b>
   (priority to the fields from the right argument) </li><li>Field removal: <b><tt>e1 \ l</tt></b> (does nothing if the
  field <b><tt>l</tt></b> is not present)</li><li>Field access: <b><tt>e1.l</tt></b></li><li>Labels are in fact Qualified Names (see )</li></ul></div><div class="span-20" id="box"><h2><a name="string">Strings</a></h2><ul><li>Strings are actually sequences of characters.</li><li>Expressions: <b><tt>&quot;abc&quot;, [ 'abc' ], [ 'a' 'b' 'c' ]</tt></b>. </li><li>Operators: <b><tt>string_of, print, dump_to_file</tt></b></li><li><b><tt>PCDATA</tt></b> means <b><tt>Char*</tt></b> inside regular expressions</li></ul></div><div class="span-20" id="box"><h2><a name="xml">XML elements</a></h2><ul><li>Expressions: <b><tt> &lt;(tag) (attr)&gt;content</tt></b></li><li>If the tag is an atom <b><tt>`X</tt></b>, it can be written
        <b><tt>X</tt></b> (without the <b><tt>(..)</tt></b>).
        Similarly, parenthesis and curly braces may be omitted
        when attr is a record <b><tt>l1=e1;...;ln=en</tt></b>
        (semicolon can also be omitted in this case).
        E.g: <b><tt>&lt;a href=&quot;abc&quot;&gt;[ 'abc' ]</tt></b>.</li><li>Types and patterns: same notations.</li><li>XPath like projection: <b><tt><i>e</i>/<i>t</i></tt></b>. For every
       XML tree in  <b><tt><i>e</i></tt></b> it returns the sequence of children
       of type  <b><tt><i>t</i></tt></b></li><li>Tree transformation: <b><tt>xtransform e with p1 -&gt; e1 | ... | pn -&gt; en</tt></b>. 
      Applies to sequences of XML trees. Unmatched elements are left unchanged and the
      transformation is recursively applied to the sequence of children of the unmatched
      element; as for transform, each branch returns a sequence
      and all the resulting sequences are concatenated together. </li><li>Operators: <b><tt>load_xml : Latin1 -&gt; AnyXml; print_xml : Any -&gt;
   Latin1; dump_xml : Any -&gt; []</tt></b></li></ul></div><div class="span-20" id="box"><h2><a name="fun">Functions</a></h2><ul><li>Expressions: 
     <ul><li>General form: <b><tt>fun f (t1-&gt;s1;...;tn-&gt;sn)
       p1 -&gt; e1 | ... | pm -&gt; em</tt></b> (<b><tt>f</tt></b> is optional) </li><li>Simple function: <b><tt>fun f (p : t) : s = e</tt></b>,
       equivalent to <b><tt>fun f (t -&gt; s) p -&gt; e</tt></b></li><li>Multiple arguments: <b><tt>fun f (p1 : t1, p2 : t2,...) : s =
  e</tt></b>, equivalent to <b><tt>fun f ((p1,p2,...):(t1,t2,...)) : s
  = e</tt></b> (note the blank spaces around colons to avoid ambiguity
     with namespaces) </li><li>Currified function: <b><tt>fun f (p1 : t1) (p2 : t2) ... : s =
     e</tt></b> (can be combined with the multiple arguments syntax).</li></ul></li><li>Types: <b><tt>t -&gt; s</tt></b></li></ul></div><div class="span-20" id="box"><h2><a name="match">Pattern matching, exceptions, ...</a></h2><ul><li>Type restriction: <b><tt>(e : t)</tt></b> (forgets any more precise
       type for <b><tt>e</tt></b>; note the blank spaces around colons to avoid ambiguity with namespaces) </li><li>Pattern matching: <b><tt>match e with p1 -&gt; e1 | ... | pn -&gt;
  en</tt></b></li><li>Local binding: <b><tt>let p = e1 in e2</tt></b>, equivalent to
   <b><tt>match e1 with p -&gt; e2</tt></b>; 
     <b><tt>let p : t = e1 in e2</tt></b> equivalent to
    <b><tt>let p = (e1 : t) in e2</tt></b></li><li>If-then-else: <b><tt>if e1 then e2 else e3</tt></b>, equivalent to
   <b><tt>match e1 with `true -&gt; e2 | `false -&gt; e3</tt></b></li><li>Exceptions: <ul><li>Raise exception: <b><tt>raise e</tt></b></li><li>Handle exception: <b><tt>try e with p1 -&gt; e1 | ... | pn -&gt;
      en</tt></b></li></ul></li></ul></div><div class="span-20" id="box"><h2><a name="type">More about types and patterns</a></h2><ul><li>Boolean connectives: <b><tt>&amp;,|,\</tt></b> (<b><tt>|</tt></b> is
  first-match). </li><li>Empty and universal types: <b><tt>Empty,Any</tt></b> or
  <b><tt>_</tt></b>.</li><li>Recursive types and patterns: <b><tt>t where T1 = t2 and ... and
  Tn = tn</tt></b>.</li><li>Capture variable: <b><tt>x</tt></b>. </li><li>Default values: <b><tt>(x := c)</tt></b>. </li></ul></div><div class="span-20" id="box"><h2><a name="ref">References</a></h2><ul><li>Type: <b><tt>ref <i>T</i></tt></b>.</li><li>Construction: <b><tt>ref <i>T</i> <i>e</i></tt></b>.</li><li>Dereferencing: <b><tt>!<i>e1</i></tt></b>.</li><li>Assignment: <b><tt><i>e1</i> := <i>e2</i></tt></b>.</li></ul></div><div class="span-20" id="box"><h2><a name="toplevel">Toplevel statements</a></h2><ul><li>Global expression to evaluate.</li><li>Global let-binding.</li><li>Global function declaration.</li><li>Type declarations: <b><tt>type T = t</tt></b>.</li><li>Global : 
   <b><tt>namespace p = &quot;...&quot;</tt></b>,
   <b><tt>namespace &quot;...&quot;</tt></b>.</li><li>Source inclusion: <b><tt>include <i>filename_string</i></tt></b>.</li><li>Debug directives: <b><tt>debug <i>directive argument</i></tt></b><br/>
      where <b><tt><i>directive</i></tt></b> is one of the following: <b><tt>accept</tt></b>, 
      <b><tt>subtype</tt></b>, <b><tt>compile</tt></b>, <b><tt>sample</tt></b>, <b><tt>filter</tt></b>. <br/>
      Use  <b><tt>#help debug</tt></b> for a short description.
      </li><li>Toplevel directives: <b><tt>#env</tt></b>, <b><tt>#quit</tt></b>,
  <b><tt>#reinit_ns</tt></b>.</li></ul></div><div class="meta"><p><a href="mailto:webmaster@cduce.org">Webmaster</a> -
  </p></div></div><div class="span-4 small last" id="vertical_bar"><div class="box"><div class="smallbox"><p>This page briefly presents the syntax of the CDuce language.</p><ul><li><a href="#id">Identifiers</a></li><li><a href="#scalars">Scalars</a></li><li><a href="#op">Operators, built-in functions</a></li><li><a href="#pair">Pairs</a></li><li><a href="#seq">Sequences</a></li><li><a href="#record">Record</a></li><li><a href="#string">Strings</a></li><li><a href="#xml">XML elements</a></li><li><a href="#fun">Functions</a></li><li><a href="#match">Pattern matching, exceptions, ...</a></li><li><a href="#type">More about types and patterns</a></li><li><a href="#ref">References</a></li><li><a href="#toplevel">Toplevel statements</a></li></ul><p>See also:</p></div></div></div><div class="span-20" id="page_footer"/></div></body></html>
