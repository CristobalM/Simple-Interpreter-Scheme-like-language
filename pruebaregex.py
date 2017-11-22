import regex

parenthesesBlock = r"\((?>[^)(]*(?R)?)*\)"

twoblock = parenthesesBlock + r"{2}"
s = "holaqlo ( sisisisi (wena wena) )(okay)"
m = regex.search(parenthesesBlock, s)
m2 = regex.search(parenthesesBlock, m.group(0))
m3 = regex.search(twoblock, s)
print m3.group(0)