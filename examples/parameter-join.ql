from Method m, Parameter p
where p = m.getAParameter() and (p.getType().getName() = "int" and p.getIndex() = 0)
select m
