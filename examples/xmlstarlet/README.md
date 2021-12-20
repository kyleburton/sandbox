`xmlstarlet` is a unix command line tool for working with XML documents.  It can
search and extract values as well as manipulate the DOM's structure.

`xmlstarlet` is great if you're doing some DevOps work and need to make some
basic customizations to some Java application's XML configuration files (if
you've got XML, some large "Enterprise" Java app is probably close by).
`xmlstarlet` isn't so great if you're doing more complicated stuff like
XSD validation or full proicessing the XML (eg: to import or export
data from a databse).

# Gotchas!

XML Namespaces.  XML and XPath both support manespacing of tags.  There are
(lots)[http://www.w3schools.com/xml/xml_namespaces.asp] of
(tutorials)[http://books.xmlschemata.org/relaxng/relax-CHP-11-SECT-1.html] and
it's too large a subject to be covere here, so I won't.

`xmlstarlet`'s made a valiant attempt to map XML, XPath and a menagerie of
operations you can perform (adding nodes, removing them, changing attributes,
moving and renaming elements) to command line switches.  `xmlstarlet`'s command
line DSL or syntax can be a bit intimidating, there are a lot of concepts here
if you're both unfamiliar with `xmlstarlet` as a tool and XML and XPath
concepts.  My advice is to make a lot of small test cases to figure out what
you want to do, how to do it, then compose them into your larger solution.

By default, `xmlstarlet` operates on the document as a whole and on all the
elements that match the selector (XPath) you've provided, if you want to
perform multiple operations to the same part of the document (eg: to add
multiple attributes to a tag), you'll need to repeat your selector or
use `$prev` (or `$xstar:prev`) to reference the elements you are attempting
to manipulate.


# References

* http://xmlstar.sourceforge.net/
* http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html#idm47077139530992
* http://stackoverflow.com/questions/5954168/how-to-insert-a-new-element-under-another-with-xmlstarlet


```bash
$ cat xml/books.xml
<library>
  <book title="Something about things" author="A. Person">
    <book-info>
      <pages>123</pages>
      <weight unit="ounces">23</weight>
      <aroma>Knowledge</aroma>
    </book-info>
  </book>
  <book title="XML Ate my Car" author="Author Machavelli Laxinica">
    <book-info>
      <pages>999</pages>
      <weight unit="parsecs">2.1831e28</weight>
      <aroma>Rancid Helvetica</aroma>
    </book-info>
  </book>
</library>
```

```bash
$ xmlstarlet  el xml/books.xml
library
library/book
library/book/book-info
library/book/book-info/pages
library/book/book-info/weight
library/book/book-info/aroma
library/book
library/book/book-info
library/book/book-info/pages
library/book/book-info/weight
library/book/book-info/aroma
``
