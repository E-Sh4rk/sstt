  $ ../../bin/test_printer.exe xhtml-strict.cd
  OK:
  type <td valign=?[ 'top' | 'baseline' | 'bottom' | 'middle' ]
           onclick=?String onkeydown=?String onmouseover=?String
           rowspan=?String ondblclick=?String dir=?[ 'rtl' | 'ltr' ]
           class=?String onmousemove=?String
           scope=?[ 'row' | 'rowgroup' | 'col' | 'colgroup' ]
           onkeypress=?String onmouseout=?String id=?String title=?String
           axis=?String onmouseup=?String colspan=?String onmousedown=?String
           headers=?String onkeyup=?String abbr=?String lang=?String
           xml:lang=?String style=?String charoff=?String char=?String
           align=?[ 'right' | 'left' | 'char' | 'center' | 'justify' ]>
           [ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl | pre |
             blockquote | address | fieldset | table | form | a | span | bdo |
             map | object | label | button | tt | i | b | big | small | em |
             strong | dfn | code | q | samp | kbd | var | cite | abbr |
             acronym | sub | sup | noscript | ins | del | hr | textarea |
             select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <th valign=?[ 'top' | 'baseline' | 'bottom' | 'middle' ] onclick=?String
  onkeydown=?String onmouseover=?String rowspan=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String
  scope=?[ 'row' | 'rowgroup' | 'col' | 'colgroup' ] onkeypress=?String
  onmouseout=?String id=?String title=?String axis=?String onmouseup=?String
  colspan=?String onmousedown=?String headers=?String onkeyup=?String
  abbr=?String lang=?String xml:lang=?String style=?String charoff=?String
  char=?String
  align=?[ 'right' | 'left' | 'char' | 'center' | 'justify' ]>[ (p | h1 | h2 |
                                                                h3 | h4 | h5 |
                                                                h6 | div | ul |
                                                                ol | dl | pre |
                                                                blockquote |
                                                                address |
                                                                fieldset |
                                                                table | form |
                                                                a | span |
                                                                bdo | map |
                                                                object |
                                                                label |
                                                                button | tt |
                                                                i | b | big |
                                                                small | em |
                                                                strong | dfn |
                                                                code | q |
                                                                samp | kbd |
                                                                var | cite |
                                                                abbr |
                                                                acronym | sub |
                                                                sup |
                                                                noscript |
                                                                ins | del |
                                                                hr | textarea |
                                                                select |
                                                                input | img |
                                                                br | script |
                                                                Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <tr valign=?[ 'top' | 'baseline' | 'bottom' | 'middle' ] onclick=?String
  onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String charoff=?String char=?String
  align=?[ 'right' | 'left' | 'char' | 'center' | 'justify' ]>[ (th | td)+ ]
  -----printed and reparsed correctly
  OK:
  type 
  <col valign=?[ 'top' | 'baseline' | 'bottom' | 'middle' ] onclick=?String
  onkeydown=?String onmouseover=?String ondblclick=?String width=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String span=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String charoff=?String char=?String
  align=?[ 'right' | 'left' | 'char' | 'center' | 'justify' ]>[  ]
  -----printed and reparsed correctly
  OK:
  type 
  <colgroup valign=?[ 'top' | 'baseline' | 'bottom' | 'middle' ]
  onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  width=?String dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String
  onkeypress=?String onmouseout=?String id=?String title=?String span=?String
  onmouseup=?String onmousedown=?String onkeyup=?String lang=?String
  xml:lang=?String style=?String charoff=?String char=?String
  align=?[ 'right' | 'left' | 'char' | 'center' | 'justify' ]>[ col* ]
  -----printed and reparsed correctly
  OK:
  type 
  <tbody valign=?[ 'top' | 'baseline' | 'bottom' | 'middle' ] onclick=?String
  onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String charoff=?String char=?String
  align=?[ 'right' | 'left' | 'char' | 'center' | 'justify' ]>[ tr+ ]
  -----printed and reparsed correctly
  OK:
  type 
  <tfoot valign=?[ 'top' | 'baseline' | 'bottom' | 'middle' ] onclick=?String
  onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String charoff=?String char=?String
  align=?[ 'right' | 'left' | 'char' | 'center' | 'justify' ]>[ tr+ ]
  -----printed and reparsed correctly
  OK:
  type 
  <thead valign=?[ 'top' | 'baseline' | 'bottom' | 'middle' ] onclick=?String
  onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String charoff=?String char=?String
  align=?[ 'right' | 'left' | 'char' | 'center' | 'justify' ]>[ tr+ ]
  -----printed and reparsed correctly
  OK:
  type 
  <caption onclick=?String onkeydown=?String onmouseover=?String
  ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String
  onkeypress=?String onmouseout=?String id=?String title=?String
  onmouseup=?String onmousedown=?String onkeyup=?String lang=?String
  xml:lang=?String
  style=?String>[ (a | span | bdo | map | object | label | button | tt | i |
                  b | big | small | em | strong | dfn | code | q | samp | kbd |
                  var | cite | abbr | acronym | sub | sup | ins | del |
                  textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <table onclick=?String onkeydown=?String onmouseover=?String
  ondblclick=?String width=?String dir=?[ 'rtl' | 'ltr' ] class=?String
  onmousemove=?String
  frame=?[ 'above' | ('l' | 'r') 'hs' | 'border' | 'box' | 'below' | 'hsides' |
         'void' | 'vsides' ]
  onkeypress=?String onmouseout=?String cellpadding=?String id=?String
  title=?String cellspacing=?String
  rules=?[ 'rows' | 'cols' | 'all' | 'none' | 'groups' ] onmouseup=?String
  border=?String onmousedown=?String onkeyup=?String summary=?String
  lang=?String xml:lang=?String
  style=?String>[ caption? tfoot tr+ | caption? tfoot tbody+ | caption? thead
                tfoot? tr+ | caption? thead tfoot? tbody+ | caption? col+ tfoot
                tr+ | caption? col+ tfoot tbody+ | caption? col+ thead 
                tfoot? tr+ | caption? col+ thead tfoot? tbody+ | caption? 
                col+ tr+ | caption? col+ tbody+ | caption? tr+ | caption?
                tbody+ | caption? colgroup+ tfoot tr+ | caption? colgroup+
                tfoot tbody+ | caption? colgroup+ thead tfoot? tr+ | caption?
                colgroup+ thead tfoot? tbody+ | caption? colgroup+ tr+ |
                caption? colgroup+ tbody+ ]
  -----printed and reparsed correctly
  OK:
  type 
  <button onclick=?String onkeydown=?String onmouseover=?String
  ondblclick=?String onblur=?String dir=?[ 'rtl' | 'ltr' ] class=?String
  onmousemove=?String onkeypress=?String onmouseout=?String id=?String
  title=?String name=?String onfocus=?String onmouseup=?String
  accesskey=?String onmousedown=?String disabled=?[ 'disabled' ] value=?String
  onkeyup=?String lang=?String xml:lang=?String style=?String tabindex=?String
  type=?[ 'reset' | 'button' | 'submit' ]>[ (p | h1 | h2 | h3 | h4 | h5 | h6 |
                                            div | ul | ol | dl | pre |
                                            blockquote | address | table |
                                            span | bdo | map | object | tt |
                                            i | b | big | small | em | strong |
                                            dfn | code | q | samp | kbd | var |
                                            cite | abbr | acronym | sub | sup |
                                            noscript | ins | del | hr | img |
                                            br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <legend onclick=?String onkeydown=?String onmouseover=?String
  ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String
  onkeypress=?String onmouseout=?String id=?String title=?String
  onmouseup=?String accesskey=?String onmousedown=?String onkeyup=?String
  lang=?String xml:lang=?String
  style=?String>[ (a | span | bdo | map | object | label | button | tt | i |
                  b | big | small | em | strong | dfn | code | q | samp | kbd |
                  var | cite | abbr | acronym | sub | sup | ins | del |
                  textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <fieldset (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (legend | p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl | pre |
         blockquote | address | <fieldset (X1)>X2 | table | form | a | span |
         bdo | map | object | label | button | tt | i | b | big | small | em |
         strong | dfn | code | q | samp | kbd | var | cite | abbr | acronym |
         sub | sup | noscript | ins | del | hr | textarea | select | input |
         img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <textarea rows=String onclick=?String onkeydown=?String onmouseover=?String
  ondblclick=?String onselect=?String onblur=?String dir=?[ 'ltr' | 'rtl' ]
  class=?String onmousemove=?String onkeypress=?String readonly=?[ 'readonly' ]
  onmouseout=?String id=?String title=?String name=?String onchange=?String
  onfocus=?String onmouseup=?String accesskey=?String onmousedown=?String
  disabled=?[ 'disabled' ] onkeyup=?String lang=?String xml:lang=?String
  style=?String tabindex=?String cols=String>String
  -----printed and reparsed correctly
  OK:
  type 
  <option onclick=?String onkeydown=?String onmouseover=?String label=?String
  ondblclick=?String dir=?[ 'ltr' | 'rtl' ] class=?String onmousemove=?String
  onkeypress=?String onmouseout=?String id=?String title=?String
  selected=?[ 'selected' ] onmouseup=?String onmousedown=?String
  disabled=?[ 'disabled' ] value=?String onkeyup=?String lang=?String
  xml:lang=?String style=?String>String
  -----printed and reparsed correctly
  OK:
  type 
  <optgroup onclick=?String onkeydown=?String onmouseover=?String label=String
  ondblclick=?String dir=?[ 'ltr' | 'rtl' ] class=?String onmousemove=?String
  onkeypress=?String onmouseout=?String id=?String title=?String
  onmouseup=?String onmousedown=?String disabled=?[ 'disabled' ]
  onkeyup=?String lang=?String xml:lang=?String style=?String>[ option+ ]
  -----printed and reparsed correctly
  OK:
  type 
  <select onclick=?String onkeydown=?String onmouseover=?String
  ondblclick=?String onblur=?String dir=?[ 'ltr' | 'rtl' ] class=?String
  onmousemove=?String onkeypress=?String onmouseout=?String id=?String
  title=?String name=?String onchange=?String onfocus=?String onmouseup=?String
  multiple=?[ 'multiple' ] onmousedown=?String size=?String
  disabled=?[ 'disabled' ] onkeyup=?String lang=?String xml:lang=?String
  style=?String tabindex=?String>[ (optgroup | option)+ ]
  -----printed and reparsed correctly
  OK:
  type 
  <input onclick=?String onkeydown=?String onmouseover=?String
  ondblclick=?String onselect=?String onblur=?String dir=?[ 'rtl' | 'ltr' ]
  class=?String onmousemove=?String onkeypress=?String readonly=?[ 'readonly' ]
  onmouseout=?String id=?String title=?String name=?String onchange=?String
  checked=?[ 'checked' ] maxlength=?String onfocus=?String onmouseup=?String
  accesskey=?String onmousedown=?String usemap=?String size=?String
  disabled=?[ 'disabled' ] value=?String onkeyup=?String accept=?String
  lang=?String xml:lang=?String style=?String alt=?String tabindex=?String
  src=?String
  type=?[ 'reset' | 'radio' | 'file' | 'text' | 'checkbox' | 'button' |
        'password' | 'submit' | 'image' | 'hidden' ]>[  ]
  -----printed and reparsed correctly
  OK:
  type 
  <label (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String onblur=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String for=?String onfocus=?String onmouseup=?String
       accesskey=?String onmousedown=?String onkeyup=?String lang=?String
       xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | <label (X1)>X2 | button | tt | i |
         b | big | small | em | strong | dfn | code | q | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <form onclick=?String onkeydown=?String onmouseover=?String
  method=?[ 'post' | 'get' ] ondblclick=?String dir=?[ 'rtl' | 'ltr' ]
  class=?String onmousemove=?String onkeypress=?String onmouseout=?String
  id=?String title=?String onreset=?String enctype=?String
  accept-charset=?String onmouseup=?String onmousedown=?String onkeyup=?String
  action=String accept=?String lang=?String xml:lang=?String style=?String
  onsubmit=?String>[ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl |
                     pre | blockquote | address | fieldset | table | noscript |
                     ins | del | hr | script)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <area onclick=?String onkeydown=?String onmouseover=?String
  shape=?[ 'rect' | 'default' | 'circle' | 'poly' ] coords=?String
  ondblclick=?String nohref=?[ 'nohref' ] onblur=?String dir=?[ 'rtl' | 'ltr' ]
  class=?String onmousemove=?String onkeypress=?String onmouseout=?String
  id=?String title=?String onfocus=?String onmouseup=?String accesskey=?String
  onmousedown=?String onkeyup=?String href=?String lang=?String
  xml:lang=?String style=?String alt=String tabindex=?String>[  ]
  -----printed and reparsed correctly
  OK:
  type 
  <map onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=String title=?String name=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl | pre |
                  blockquote | address | fieldset | table | form | noscript |
                  ins | del | hr | script)+ |
                area+ ]
  -----printed and reparsed correctly
  OK:
  type <img
                                                                     onclick=?String
                                                                     onkeydown=?String
                                                                     onmouseover=?String
                                                                     ondblclick=?String
                                                                     width=?String
                                                                     dir=?
                                                                     [ 'rtl' |
                                                                     'ltr' ]
                                                                     class=?String
                                                                     onmousemove=?String
                                                                     onkeypress=?String
                                                                     onmouseout=?String
                                                                     id=?String
                                                                     title=?String
                                                                     longdesc=?String
                                                                     height=?String
                                                                     ismap=?
                                                                     [ 'ismap' ]
                                                                     onmouseup=?String
                                                                     onmousedown=?String
                                                                     usemap=?String
                                                                     onkeyup=?String
                                                                     lang=?String
                                                                     xml:lang=?String
                                                                     style=?String
                                                                     alt=String
                                                                     src=String>
                                                                     [  ]
  -----printed and reparsed correctly
  OK:
  type 
  <param valuetype=?[ 'ref' | 'data' | 'object' ] id=?String name=?String
  value=?String type=?String>[  ]
  -----printed and reparsed correctly
  OK:
  type 
  <object (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       declare=?[ 'declare' ] standby=?String ondblclick=?String width=?String
       dir=?[ 'rtl' | 'ltr' ] class=?String codebase=?String
       onmousemove=?String classid=?String onkeypress=?String
       onmouseout=?String id=?String title=?String name=?String height=?String
       onmouseup=?String onmousedown=?String usemap=?String onkeyup=?String
       archive=?String lang=?String xml:lang=?String style=?String
       codetype=?String tabindex=?String type=?String data=?String } and
  X2 = [ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl | pre |
         blockquote | address | fieldset | table | form | a | span | bdo |
         map | <object (X1)>X2 | label | button | tt | i | b | big | small |
         em | strong | dfn | code | q | samp | kbd | var | cite | abbr |
         acronym | sub | sup | noscript | ins | del | hr | textarea | select |
         input | img | br | param | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <small (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         <small (X1)>X2 | em | strong | dfn | code | q | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <big (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b |
         <big (X1)>X2 | small | em | strong | dfn | code | q | samp | kbd |
         var | cite | abbr | acronym | sub | sup | ins | del | textarea |
         select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <b (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i |
         <b (X1)>X2 | big | small | em | strong | dfn | code | q | samp | kbd |
         var | cite | abbr | acronym | sub | sup | ins | del | textarea |
         select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <i (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | <i (X1)>X2 |
         b | big | small | em | strong | dfn | code | q | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <tt (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | <tt (X1)>X2 | i |
         b | big | small | em | strong | dfn | code | q | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <sup (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | dfn | code | q | samp | kbd | var | cite |
         abbr | acronym | sub | <sup (X1)>X2 | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <sub (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | dfn | code | q | samp | kbd | var | cite |
         abbr | acronym | <sub (X1)>X2 | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <q (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String cite=?String onmouseup=?String onmousedown=?String
       onkeyup=?String lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | dfn | code | <q (X1)>X2 | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <acronym (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | dfn | code | q | samp | kbd | var | cite |
         abbr | <acronym (X1)>X2 | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <abbr (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | dfn | code | q | samp | kbd | var | cite |
         <abbr (X1)>X2 | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <cite (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | dfn | code | q | samp | kbd | var |
         <cite (X1)>X2 | abbr | acronym | sub | sup | ins | del | textarea |
         select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <var (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | dfn | code | q | samp | kbd | <var (X1)>X2 |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <kbd (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | dfn | code | q | samp | <kbd (X1)>X2 | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <samp (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | dfn | code | q | <samp (X1)>X2 | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <code (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | dfn | <code (X1)>X2 | q | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <dfn (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | strong | <dfn (X1)>X2 | code | q | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <strong (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | em | <strong (X1)>X2 | dfn | code | q | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <em (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | bdo | map | object | label | button | tt | i | b | big |
         small | <em (X1)>X2 | strong | dfn | code | q | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <br class=?String id=?String title=?String style=?String>[  ]
  -----printed and reparsed correctly
  OK:
  type 
  <bdo (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | span | <bdo (X1)>X2 | map | object | label | button | tt | i |
         b | big | small | em | strong | dfn | code | q | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <span (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (a | <span (X1)>X2 | bdo | map | object | label | button | tt | i |
         b | big | small | em | strong | dfn | code | q | samp | kbd | var |
         cite | abbr | acronym | sub | sup | ins | del | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <a onclick=?String onkeydown=?String onmouseover=?String
  shape=?[ 'rect' | 'default' | 'circle' | 'poly' ] coords=?String
  ondblclick=?String charset=?String onblur=?String dir=?[ 'rtl' | 'ltr' ]
  class=?String onmousemove=?String onkeypress=?String onmouseout=?String
  rel=?String id=?String title=?String name=?String hreflang=?String
  onfocus=?String onmouseup=?String accesskey=?String onmousedown=?String
  onkeyup=?String rev=?String href=?String lang=?String xml:lang=?String
  style=?String tabindex=?String
  type=?String>[ (span | bdo | map | object | label | button | tt | i | b |
                 big | small | em | strong | dfn | code | q | samp | kbd |
                 var | cite | abbr | acronym | sub | sup | ins | del |
                 textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <del (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String cite=?String onmouseup=?String datetime=?String
       onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
       style=?String } and
  X2 = [ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl | pre |
         blockquote | address | fieldset | table | form | a | span | bdo |
         map | object | label | button | tt | i | b | big | small | em |
         strong | dfn | code | q | samp | kbd | var | cite | abbr | acronym |
         sub | sup | noscript | ins | <del (X1)>X2 | hr | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <ins (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String cite=?String onmouseup=?String datetime=?String
       onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
       style=?String } and
  X2 = [ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl | pre |
         blockquote | address | fieldset | table | form | a | span | bdo |
         map | object | label | button | tt | i | b | big | small | em |
         strong | dfn | code | q | samp | kbd | var | cite | abbr | acronym |
         sub | sup | noscript | <ins (X1)>X2 | del | hr | textarea | select |
         input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <blockquote (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String cite=?String onmouseup=?String onmousedown=?String
       onkeyup=?String lang=?String xml:lang=?String style=?String } and
  X2 = [ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl | pre |
         <blockquote (X1)>X2 | address | fieldset | table | form | noscript |
         ins | del | hr | script)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <pre onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (a | span | bdo | map | label | button | tt | i | b | big |
                  small | em | strong | dfn | code | q | samp | kbd | var |
                  cite | abbr | acronym | sub | sup | ins | del | textarea |
                  select | input | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <hr onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[  ]
  -----printed and reparsed correctly
  OK:
  type <address
                                                                  onclick=?String
                                                                  onkeydown=?String
                                                                  onmouseover=?String
                                                                  ondblclick=?String
                                                                  dir=?
                                                                  [ 'rtl' |
                                                                  'ltr' ]
                                                                  class=?String
                                                                  onmousemove=?String
                                                                  onkeypress=?String
                                                                  onmouseout=?String
                                                                  id=?String
                                                                  title=?String
                                                                  onmouseup=?String
                                                                  onmousedown=?String
                                                                  onkeyup=?String
                                                                  lang=?String
                                                                  xml:lang=?String
                                                                  style=?String>
                                                                  [ (a | span |
                                                                    bdo | map |
                                                                    object |
                                                                    label |
                                                                    button |
                                                                    tt | i |
                                                                    b | big |
                                                                    small |
                                                                    em |
                                                                    strong |
                                                                    dfn |
                                                                    code | q |
                                                                    samp |
                                                                    kbd | var |
                                                                    cite |
                                                                    abbr |
                                                                    acronym |
                                                                    sub | sup |
                                                                    ins | del |
                                                                    textarea |
                                                                    select |
                                                                    input |
                                                                    img | br |
                                                                    script |
                                                                    Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <dd onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl | pre |
                  blockquote | address | fieldset | table | form | a | span |
                  bdo | map | object | label | button | tt | i | b | big |
                  small | em | strong | dfn | code | q | samp | kbd | var |
                  cite | abbr | acronym | sub | sup | noscript | ins | del |
                  hr | textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <dt onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (a | span | bdo | map | object | label | button | tt | i |
                  b | big | small | em | strong | dfn | code | q | samp | kbd |
                  var | cite | abbr | acronym | sub | sup | ins | del |
                  textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <dl onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (dt | dd)+ ]
  -----printed and reparsed correctly
  OK:
  type 
  <li onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl | pre |
                  blockquote | address | fieldset | table | form | a | span |
                  bdo | map | object | label | button | tt | i | b | big |
                  small | em | strong | dfn | code | q | samp | kbd | var |
                  cite | abbr | acronym | sub | sup | noscript | ins | del |
                  hr | textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <ol onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ li+ ]
  -----printed and reparsed correctly
  OK:
  type <ul
                                                                     onclick=?String
                                                                     onkeydown=?String
                                                                     onmouseover=?String
                                                                     ondblclick=?String
                                                                     dir=?
                                                                     [ 'rtl' |
                                                                     'ltr' ]
                                                                     class=?String
                                                                     onmousemove=?String
                                                                     onkeypress=?String
                                                                     onmouseout=?String
                                                                     id=?String
                                                                     title=?String
                                                                     onmouseup=?String
                                                                     onmousedown=?String
                                                                     onkeyup=?String
                                                                     lang=?String
                                                                     xml:lang=?String
                                                                     style=?String>
                                                                     [ 
                                                                     li+ ]
  -----printed and reparsed correctly
  OK:
  type 
  <h6 onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (a | span | bdo | map | object | label | button | tt | i |
                  b | big | small | em | strong | dfn | code | q | samp | kbd |
                  var | cite | abbr | acronym | sub | sup | ins | del |
                  textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <h5 onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (a | span | bdo | map | object | label | button | tt | i |
                  b | big | small | em | strong | dfn | code | q | samp | kbd |
                  var | cite | abbr | acronym | sub | sup | ins | del |
                  textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <h4 onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (a | span | bdo | map | object | label | button | tt | i |
                  b | big | small | em | strong | dfn | code | q | samp | kbd |
                  var | cite | abbr | acronym | sub | sup | ins | del |
                  textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <h3 onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (a | span | bdo | map | object | label | button | tt | i |
                  b | big | small | em | strong | dfn | code | q | samp | kbd |
                  var | cite | abbr | acronym | sub | sup | ins | del |
                  textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <h2 onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (a | span | bdo | map | object | label | button | tt | i |
                  b | big | small | em | strong | dfn | code | q | samp | kbd |
                  var | cite | abbr | acronym | sub | sup | ins | del |
                  textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <h1 onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (a | span | bdo | map | object | label | button | tt | i |
                  b | big | small | em | strong | dfn | code | q | samp | kbd |
                  var | cite | abbr | acronym | sub | sup | ins | del |
                  textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <p onclick=?String onkeydown=?String onmouseover=?String ondblclick=?String
  dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String onkeypress=?String
  onmouseout=?String id=?String title=?String onmouseup=?String
  onmousedown=?String onkeyup=?String lang=?String xml:lang=?String
  style=?String>[ (a | span | bdo | map | object | label | button | tt | i |
                  b | big | small | em | strong | dfn | code | q | samp | kbd |
                  var | cite | abbr | acronym | sub | sup | ins | del |
                  textarea | select | input | img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <div (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (p | h1 | h2 | h3 | h4 | h5 | h6 | <div (X1)>X2 | ul | ol | dl | pre |
         blockquote | address | fieldset | table | form | a | span | bdo |
         map | object | label | button | tt | i | b | big | small | em |
         strong | dfn | code | q | samp | kbd | var | cite | abbr | acronym |
         sub | sup | noscript | ins | del | hr | textarea | select | input |
         img | br | script | Char)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <body onclick=?String onkeydown=?String onmouseover=?String
  ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String onmousemove=?String
  onunload=?String onkeypress=?String onmouseout=?String id=?String
  title=?String onmouseup=?String onmousedown=?String onkeyup=?String
  lang=?String xml:lang=?String style=?String
  onload=?String>[ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl |
                   pre | blockquote | address | fieldset | table | form |
                   noscript | ins | del | hr | script)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <noscript (X1)>X2 where
  X1 = { onclick=?String onkeydown=?String onmouseover=?String
       ondblclick=?String dir=?[ 'rtl' | 'ltr' ] class=?String
       onmousemove=?String onkeypress=?String onmouseout=?String id=?String
       title=?String onmouseup=?String onmousedown=?String onkeyup=?String
       lang=?String xml:lang=?String style=?String } and
  X2 = [ (p | h1 | h2 | h3 | h4 | h5 | h6 | div | ul | ol | dl | pre |
         blockquote | address | fieldset | table | form | <noscript (X1)>X2 |
         ins | del | hr | script)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <script defer=?[ 'defer' ] charset=?String id=?String src=?String
  type=String>String
  -----printed and reparsed correctly
  OK:
  type <style
                                                                  dir=?
                                                                  [ 'rtl' |
                                                                  'ltr' ]
                                                                  id=?String
                                                                  title=?String
                                                                  media=?String
                                                                  lang=?String
                                                                  xml:lang=?String
                                                                  type=String>String
  -----printed and reparsed correctly
  OK:
  type 
  <link onclick=?String onkeydown=?String onmouseover=?String
  ondblclick=?String charset=?String dir=?[ 'rtl' | 'ltr' ] class=?String
  onmousemove=?String onkeypress=?String onmouseout=?String rel=?String
  id=?String title=?String hreflang=?String media=?String onmouseup=?String
  onmousedown=?String onkeyup=?String rev=?String href=?String lang=?String
  xml:lang=?String style=?String type=?String>[  ]
  -----printed and reparsed correctly
  OK:
  type 
  <meta dir=?[ 'rtl' | 'ltr' ] content=String id=?String name=?String
  http-equiv=?String scheme=?String lang=?String xml:lang=?String>[  ]
  -----printed and reparsed correctly
  OK:
  type 
  <base id=?String href=String>[  ]
  -----printed and reparsed correctly
  OK:
  type 
  <title dir=?[ 'rtl' | 'ltr' ] id=?String lang=?String xml:lang=?String>String
  -----printed and reparsed correctly
  OK:
  type 
  <head profile=?String dir=?[ 'rtl' | 'ltr' ] id=?String lang=?String
  xml:lang=?String>[ (object | link | meta | style | script)* title
                   (object | link | meta | style | script)* |
                   (object | link | meta | style | script)* title
                   (object | link | meta | style | script)* base
                   (object | link | meta | style | script)* |
                   (object | link | meta | style | script)* base
                   (object | link | meta | style | script)* title
                   (object | link | meta | style | script)* ]
  -----printed and reparsed correctly
  OK:
  type 
  <html dir=?[ 'rtl' | 'ltr' ] id=?String
  xmlns=[ 'http://www.w3.org/1999/xhtml' ] lang=?String
  xml:lang=?String>[ head body ]
  -----printed and reparsed correctly
