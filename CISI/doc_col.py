import re
import copy

state_title = 1
state_no = 2
state_content = 3
state_index = 4
state = 0

f = open("cisi.all","r")
contents = f.read()
lines = contents.split('\n')
f.close()

document = []
cur_doc = {}
for line in lines:
    if line.startswith('.I '):
        document.append(copy.copy(cur_doc))
        cur_doc['no'] = line[3:]
    elif line.startswith('.T'):
        state = state_title
        cur_doc['title'] = line[2:]
    elif line.startswith('.W'):
        state = state_content
        cur_doc['content'] = line[2:]
    elif line.startswith('.X'):
        state = state_index
    elif state == state_title:
        cur_doc['title'] = cur_doc['title'] + line
    elif state == state_content:
        cur_doc['content'] = cur_doc['content'] + line

document = document[1:]
document.append(cur_doc)

for doc in document:
    print(doc)
    file = open('doc/'+doc['no'],'w')
    file.write(doc['title'] + '\n')
    file.write(doc['content'])
    file.close()
