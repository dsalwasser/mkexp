#!/bin/bash

. "$script_pwd/../partitioners/inc/git.sh"
. "$script_pwd/../partitioners/inc/kagen-partitioner.sh"

Fetch() {
    local -n fetch_args=$1

    if (( ${fetch_args[install_disk_driver]} )); then 
        FetchDiskDriver fetch_args
    fi
    if (( ${fetch_args[install_kagen_driver]} )); then 
        GenericKaGenPartitionerFetch fetch_args
    fi
}

FetchDiskDriver() {
    local -n fetch_disk_driver_args=$1

    if [[ -v KAMINPAR_PRIVATE_REPOSITORY_URL ]]; then
        GenericGitFetch fetch_disk_driver_args $KAMINPAR_PRIVATE_REPOSITORY_URL "disk_driver_src"
    else 
        GenericGitFetch fetch_disk_driver_args "https://github.com/KaHIP/KaMinPar.git" "disk_driver_src"
    fi
}

Install() {
    local -n install_args=$1
    
    if (( ${install_args[install_disk_driver]} )); then 
        InstallDiskDriver install_args
    fi
    if (( ${install_args[install_kagen_driver]} )); then 
        GenericKaGenPartitionerInstall install_args -DBUILD_KAMINPAR=On "KaMinPar"
    fi
}

InstallDiskDriver() {
    local -n install_disk_driver_args=$1

    src_dir="${install_disk_driver_args[disk_driver_src]}"

    echo -e "Build algorithm '$ALGO_COLOR${install_disk_driver_args[algorithm]}$NO_COLOR' in directory '$src_dir'"
    echo "  - System-specific CMake options: $CUSTOM_CMAKE_FLAGS"
    echo -e "  - Algorithm-specific CMake options: $ARGS_COLOR${install_disk_driver_args[algorithm_build_options]}$NO_COLOR"
    echo ""

    if [[ -v KAMINPAR_BENCHMARK_TARGET ]]; then
        Prefixed cmake -S "$src_dir" \
        -B "$src_dir/build" \
        -DCMAKE_BUILD_TYPE=Release \
        -DKAMINPAR_BUILD_BENCHMARKS=On \
        $CUSTOM_CMAKE_FLAGS \
        ${install_disk_driver_args[algorithm_build_options]}
        Prefixed cmake --build "$src_dir/build" --target $KAMINPAR_BENCHMARK_TARGET --parallel
        Prefixed cp "$src_dir/build/apps/benchmarks/$KAMINPAR_BENCHMARK_TARGET" "${install_disk_driver_args[disk_driver_bin]}"
    else
        Prefixed cmake -S "$src_dir" \
        -B "$src_dir/build" \
        -DCMAKE_BUILD_TYPE=Release \
        $CUSTOM_CMAKE_FLAGS \
        ${install_disk_driver_args[algorithm_build_options]}
        Prefixed cmake --build "$src_dir/build" --target KaMinPar --parallel
        Prefixed cp "$src_dir/build/apps/KaMinPar" "${install_disk_driver_args[disk_driver_bin]}"
    fi
}

InvokeFromDisk() {
    local -n invoke_from_disk_args=$1
    
    graph="${invoke_from_disk_args[graph]}"
    [[ -f "$graph.graph" ]] && graph="$graph.graph"
    [[ -f "$graph.metis" ]] && graph="$graph.metis"
    [[ -f "$graph.parhip" ]] && graph="$graph.parhip"

    if [[ "${invoke_from_disk_args[print_partitioner]}" == "1" ]]; then 
        >&2 echo -e "Generating calls for algorithm '$ALGO_COLOR${invoke_from_disk_args[algorithm]}$NO_COLOR', from disk, via the binary:"
        >&2 echo "  - Binary: ${invoke_from_disk_args[bin]}"
        >&2 echo "  - Generated arguments: "
        >&2 echo -e "      -G $ARGS_COLOR$graph$NO_COLOR"
        >&2 echo -e "      -k $ARGS_COLOR${invoke_from_disk_args[k]}$NO_COLOR"
        >&2 echo -e "      -e $ARGS_COLOR${invoke_from_disk_args[epsilon]}$NO_COLOR"
        >&2 echo -e "      --seed=$ARGS_COLOR${invoke_from_disk_args[seed]}$NO_COLOR"
        >&2 echo -e "      -t $ARGS_COLOR${invoke_from_disk_args[num_threads]}$NO_COLOR"
        if [[ ! -v KAMINPAR_BENCHMARK_TARGET ]]; then
            >&2 echo -e "      -T"
            if [[ -v KAMINPAR_PRIVATE_REPOSITORY_URL ]]; then
                >&2 echo -e "      -H"
            fi
        fi
        >&2 echo -e "  - Specified arguments: $ARGS_COLOR${invoke_from_disk_args[algorithm_arguments]}$NO_COLOR"
        >&2 echo "[...]"
        >&2 echo ""
    fi

    if [[ -f "$graph" ]]; then
        echo -n "/usr/bin/time -v "
        echo -n "${invoke_from_disk_args[bin]} "
        echo -n "-G $graph "
        echo -n "-k ${invoke_from_disk_args[k]} "
        echo -n "-e ${invoke_from_disk_args[epsilon]} "
        echo -n "--seed=${invoke_from_disk_args[seed]} "
        echo -n "-t ${invoke_from_disk_args[num_threads]} "
        if [[ ! -v KAMINPAR_BENCHMARK_TARGET ]]; then
            echo -n "-T "
            if [ -v KAMINPAR_PRIVATE_REPOSITORY_URL ] && [ "${invoke_from_disk_args[algorithm_version]}" == "latest" ] && [[ "${invoke_from_disk_args[algorithm_build_options]}" != *"-DKAMINPAR_ENABLE_HEAP_PROFILING=Off"* ]]; then
                echo -n "-H "
            fi
        fi
        echo -n "${invoke_from_disk_args[algorithm_arguments]}"
        echo ""
    else 
        >&2 echo "Warning: Graph $graph does not exist; skipping instance"
        return 1
    fi
}

InvokeFromKaGen() {
    GenericKaGenPartitionerInvokeFromKaGen $1
}

ReportVersion() {
    GenericGitReportVersion $1
}
