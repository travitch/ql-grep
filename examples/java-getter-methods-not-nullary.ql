from Method m where m.getName().regexpMatch("^get.*") and count(m.getAParameter()) > 0 select m
