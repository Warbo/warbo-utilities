#!/usr/bin/env bash

function metalArchiveFor {
    CACHED="$CACHE_DIR/$INIT/$1"
    if [[ -f "$CACHED" ]]
    then
        true
    else
        echo "Searching for '$1' on metal-archives.com" 1>&2
        sleep 1
        curl --get --data-urlencode "field=name"      \
             --data-urlencode "query=$1" \
             "http://www.metal-archives.com/search/ajax-band-search/" > "$CACHED"
    fi
    [[ -f "$CACHED" ]] || {
        echo "Error: Couldn't find metal-archives data for '$1'" 1>&2
        return 1
    }
    echo "$CACHED"
}

function stripSpace {
    sed -e 's/[[:space:]]*$//g' | sed -e 's/^[[:space:]]*//g'
}

function haveMetalArchive {
    # If the given directory name ends in (foo) split it off as a country code
    if echo "$1" | grep '([^)]*)$' > /dev/null
    then
        BANDNAME=$(echo "$1" | rev | cut -d '(' -f 2- | rev | stripSpace)
        COUNTRY=$(echo "$1" | rev | cut -d '(' -f 1  | rev | tr -d ')' | stripSpace)
    else
        BANDNAME="$1"
        COUNTRY=""
    fi

    # Look up the band on metal archives
    ARCHIVE=$(metalArchiveFor "$BANDNAME") || return 1

    # If we found one match, assume it's exact (for now)
    MATCHES=$(jq '.iTotalRecords' < "$ARCHIVE")
    if [[ "$MATCHES" -eq 1 ]]
    then
        return 0
    fi

    # If we found no matches, there's nothing we can do
    if [[ "$MATCHES" -eq 0 ]]
    then
        # Remove error message when we have more backends
         echo "Error: No matches for '$BANDNAME' on metal-archives" 1>&2
         return 1
    fi

    # Nothing to distinguish between multiple matches
    if [[ "$MATCHES" -gt 0 ]] && [[ -z "$COUNTRY" ]]
    then
        echo "Error: $MATCHES matches for '$BANDNAME' on metal archives (maybe add country?)" 1>&2
        return 1
    fi

    # See if we have any matching countries
    CNT=$(codeToCountry "$COUNTRY")
    CNTRESULTS=$(jq ".aaData | map(select(.[2] == \"$CNT\"))" < "$ARCHIVE")
    CNTMATCHES=$(echo "$CNTRESULTS" | jq 'length')
    if [[ "$CNTMATCHES" -eq 1 ]]
    then
        return 0
    fi

    if [[ "$CNTMATCHES" -eq 0 ]]
    then
        echo "Error: No results for '$BANDNAME' from '$CNT'" 1>&2
        return 1
    fi

    echo "Error: $CNTMATCHES matches for '$BANDNAME' from '$CNT'" 1>&2
    return 1
}

function codeToCountry {
    LOWER=$(echo "$1" | tr '[[:upper:]]' '[[:lower:]]')
    case "$LOWER" in
        bel)
            echo "Belgium"
            ;;
        swe)
            echo "Sweden"
            ;;
        swedish)
            echo "Sweden"
            ;;
        can)
            echo "Canada"
            ;;
        fra)
            echo "France"
            ;;
        uk)
            echo "United Kingdom"
            ;;
        us)
            echo "United States"
            ;;
        ger)
            echo "Germany"
            ;;
        german)
            echo "Germany"
            ;;
        jap)
            echo "Japan"
            ;;
        jpn)
            echo "Japan"
            ;;
        cze)
            echo "Czech Republic"
            ;;
        usa)
            echo "United States"
            ;;
        nld)
            echo "Netherlands"
            ;;
        pol)
            echo "Poland"
            ;;
        dnk)
            echo "Denmark"
            ;;
        grc)
            echo "Greece"
            ;;
        it)
            echo "Italy"
            ;;
        arg)
            echo "Argentina"
            ;;
        nor)
            echo "Norway"
            ;;
        *)
            echo "$1"
            ;;
    esac
}

[[ -n "$INIT" ]] || {
    echo "No INIT set, aborting" 1>&2
    exit 1
}

CACHE_DIR="$PWD/.artist_name_cache"
mkdir -p "$CACHE_DIR"

haveMetalArchive "$1"
