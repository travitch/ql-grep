// Match all methods (usually Java) with names that begin with "get" and that have at least one parameter
from Method m where m.getName().regexpMatch("^get.*") and count(m.getAParameter()) > 0 select m
