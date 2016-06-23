#!/usr/bin/env bash

function stripSpace {
    sed -e 's/[[:space:]]*$//g' | sed -e 's/^[[:space:]]*//g'
}

function codeToCountry {
    LOWER=$(echo "$1" | tr '[:upper:]' '[:lower:]')
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

# If the given directory name ends in (foo) split it off as a country code
if echo "$1" | grep '([^)]*)$' > /dev/null
then
    BANDNAME=$(echo "$1" | rev | cut -d '(' -f 2- | rev | stripSpace)
    COUNTRY=$(echo "$1" | rev | cut -d '(' -f 1  | rev | tr -d ')' | stripSpace)
else
    BANDNAME="$1"
    COUNTRY=""
fi
CNT=$(codeToCountry "$COUNTRY")

echo -e "$BANDNAME\t$CNT"
