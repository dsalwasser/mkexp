if [[ $mode == "init" ]]; then 
    cp "$ROOT/examples/Default" "$PWD/Experiment"
    exit 0
fi
if [[ $mode == "init-mtkahypar-supermuc" ]]; then 
    cp "$ROOT/examples/MtKaHyPar-SuperMUC" "$PWD/Experiment"
    exit 0
fi
if [[ $mode == "init-kaminpar-memory-optimizations" ]]; then
    cp "$ROOT/examples/KaMinPar-Memory-Optimizations" "$PWD/Experiment"
    exit 0
fi
if [[ $mode == "init-kaminpar-label-propagation" ]]; then 
    cp "$ROOT/examples/KaMinPar-Label-Propagation" "$PWD/Experiment"
    exit 0
fi
if [[ $mode == "init-kaminpar-graph-compression" ]]; then 
    cp "$ROOT/examples/KaMinPar-Graph-Compression" "$PWD/Experiment"
    exit 0
fi
if [[ $mode == "init-kaminpar-graph-compression-encoding" ]]; then 
    cp "$ROOT/examples/KaMinPar-Graph-Compression-Encoding" "$PWD/Experiment"
    exit 0
fi
if [[ $mode == "init-kaminpar-graph-compression-varint-codec" ]]; then 
    cp "$ROOT/examples/KaMinPar-Graph-Compression-VarInt-Codec" "$PWD/Experiment"
    exit 0
fi
