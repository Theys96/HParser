import NewParser

input = [(BRKTOPEN,"!"),(BRKTOPEN,"("),(ONE,"1"),(BRKTCLOSE,")"),(BRKTCLOSE,"!")]

tree = parseTree input
