define pxml
  p xmlElemDump(stdout, $arg0, xmlDocGetRootElement($arg0))
end
document pxml
Print the tree of an already opened XML document (i.e., an xmlDocPtr) to the
screen.  This will probably be several pages long.
E.g., given: xmlDocPtr doc = xmlParseFile(infile);
use: pxml doc
end

# Helpers for GLib's Linked List
define phead
  set $ptr = $arg1
  plistdata $arg0
end
document phead
Print the first element of a list. E.g., given the declaration
  Glist *datalist;
  g_list_add(datalist, "Hello");
view the list with something like
gdb> phead char datalist
gdb> pnext char
gdb> pnext char
This macro defines $ptr as the current pointed-to list struct,
and $pdata as the data in the list element.
end

define pnext
  set $ptr = $ptr->next
  plistdata $arg0
end
document pnext
You need to call phead first; that will set $ptr.
This macro will step forwrd in the list, then show the value at
that next element.  Give the type of the list data as teh only argument.

This macro defines $ptr as the current pointed-to list struct, and
$pdata as the data in that list element.
end

define plistdata
  if $ptr
    set $pdata = $ptr->data
  else
    set $pdata = 0
  end

  if $pdata
    p ($arg0*)$pdata
  else
    p "NULL"
  end
end
document plistdata
this is intended to be used by phead and pnext, q.v. It sets $pdata and prints its value.
end
