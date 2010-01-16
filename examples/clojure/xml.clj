"

Hello
I have a simple task of reading an XML structure, manipulate part of
it and writing it back to XML.
For example, adding 1$ for each book with a year element after 2005 in
the following example:

<?xml version="1.0" encoding="UTF-8"?>
<bookstore>
 <book category="COOKING">
   <title lang="en">Everyday Italian</title>
   <author>Giada De Laurentiis</author>
   <year>2005</year>
   <price>30.00</price>
 </book>
 <book category="CHILDREN">
   <title lang="en">Harry Potter</title>
   <author>J K. Rowling</author>
   <year>2006</year>
   <price>29.99</price>
 </book>
</bookstore>

clojure.contrib.zip-filter.xml is getting me close to this, but I
still do not see how can I use it (or other library) to modify values.
What would be the idiomatic (and easiest) way to do that?
I apologize in advance if this is too trivial.

Thanks
Tzach

--------------------------------------------------------------------------------
Tzach,
I'd start will clojure.xml.  At a very high level, my program would
look like this

1.  Load the xml file with clojure.xml/parse
2.  Apply your filtering code with something like map-if (see below)

(defn map-if [pred f coll]
    (map #(if (pred %) (f %) %) coll))

3.  Use clojure.contrib.prxml to output the data."
