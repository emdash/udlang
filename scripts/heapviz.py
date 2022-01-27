import json
import os
import sys

"""Quick-and-dirty heap visualizer for the g-machine"""

DIR = "heaps"

try:
    os.mkdir(DIR)
except FileExistsError:
    pass

dump = [json.loads(line) for line in sys.stdin]

def entries(item, *fields):
    return [(field, item[field]) for field in fields]

def item_edges(item):
    t = item["type"]
    if   t == "value":  return []
    elif t == "app":    return entries(item, "left", "right")
    elif t == "global": return []
    elif t == "ind":    return entries(item, "next")
    elif t == "data":   return [
        ("f%d" % i, v) for i, v in enumerate(item["data"])
    ]

def item_label(item):
    t = item["type"]
    if   t == "value":  return "value: " + str(item["value"])
    elif t == "app":    return "app"
    elif t == "global": return "global: " + item["name"]
    elif t == "ind":    return "ind"

def style_node(addr, item, edges):
    items = [item_label(item)] + ["<%s> %s" % (f, f) for (f, _) in edges]
    label = (" | ").join(items)
    return "%s [id=%s label=%s];" % (addr, addr, json.dumps(label))

def make_heap_graph(heap, f):
    print("digraph {", file=f)
    print("node [shape=record]", file=f)
    for addr, item in heap.items():
        edges = item_edges(item)
        print(style_node(addr, item, edges), file=f)
        for field, ref in edges:
            print("%s:%s -> %r;" % (addr, field, ref), file=f)
    print("}", file=f)

print("const dump = [");
for (i, state) in enumerate(dump):
    heap = state["heap"]
    dotfile = "%s/heap%d.dot" % (DIR, i)
    imgfile = "%s/heap%d.svg" % (DIR, i)

    # generate heap image
    make_heap_graph(heap, open(dotfile, "w"))
    os.system("dot -Tsvg %s > %s" % (dotfile, imgfile))

    # insert the image path into the debug trace
    state["heap"] = {"img": imgfile, "data": heap}
    json.dump(state, sys.stdout)
    print(",")
print("];")
