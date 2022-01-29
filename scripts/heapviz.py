import json
import os
import sys

"""Quick-and-dirty heap visualizer for the g-machine"""

DIR = "heaps"

try:
    os.mkdir(DIR)
except FileExistsError:
    pass

def debug(expr, *shit):
    print(expr, *shit, file=sys.stderr)
    return expr

def entries(item, *fields):
    return [(field, item[field]) for field in fields]

def style_node(addr, item):
    label = (" | ").join([
        "%s:%s" % (item["type"], item["label"]),
        *["<%s> %s" % (f, f) for (f, _) in item["refs"]]
    ])
    return "%s [id=%s label=%s];" % (addr, addr, json.dumps(label))

def make_heap_graph(heap, f):
    print("digraph {", file=f)
    print("node [shape=record]", file=f)
    for (addr, item) in heap.items():
        print(style_node(addr, item), file=f)
        for field, ref in item["refs"]:
            print("%s:%s -> %r;" % (addr, field, ref), file=f)
    print("}", file=f)

print("const dump = [");
for (i, line) in enumerate(sys.stdin):
    state = json.loads(line)
    heap = state["heap"]

    dotfile = "%s/heap%d.dot" % (DIR, i)
    imgfile = "%s/heap%d.svg" % (DIR, i)
    make_heap_graph(heap, open(dotfile, "w"))
    os.system("dot -Tsvg %s > %s" % (dotfile, imgfile))

    # insert the image path into the debug trace
    state["heap"] = {"img": imgfile, "data": heap}
    json.dump(state, sys.stdout)
    print(",")
print("];")
