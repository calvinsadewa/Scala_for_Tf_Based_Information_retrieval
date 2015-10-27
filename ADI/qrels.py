import re


f = open("qrels.text","r")
contents = f.read()
list = contents.split('\n')
magic = []
p = re.compile('\d+')
for content in list:
    hs = p.findall(content)
    if len(hs) > 2 :
        magic.append((hs[0],hs[1]))


dictionary = dict()
values = set(map(lambda x:x[0], magic))
for x in values:
    hasil = [y[1] for y in magic if y[0]==x]
    file = open('relevance/'+x,'w')
    for i in hasil:
        file.write(i + '\n')
    file.close()


f.close()
