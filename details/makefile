
ILLUSTRATIONS = dep_graph.png dep_graph_empty.png

DOTSCRIPT       = ./makedot.sh
DOTSCRIPT_FLAGS = 

all: $(ILLUSTRATIONS)

%.png: %.dot
	$(DOTSCRIPT) $(DOTSCRIPT_FLAGS) $<
