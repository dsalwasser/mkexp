if [[ $mode == "init" ]]; then 
    cp "$ROOT/examples/Default" "$PWD/Experiment"
    exit 0
fi
if [[ $mode == "init-mtkahypar-supermuc" ]]; then 
    cp "$ROOT/examples/MtKaHyPar-SuperMUC" "$PWD/Experiment"
    exit 0
fi
if [[ $mode == "init-kaminpar-graph-compression" ]]; then 
    cp "$ROOT/examples/KaMinPar-Graph-Compression" "$PWD/Experiment"
    exit 0
fi
if [[ $mode == "init-kaminpar-label-propagation" ]]; then 
    cp "$ROOT/examples/KaMinPar-Label-Propagation" "$PWD/Experiment"
    exit 0
fi
