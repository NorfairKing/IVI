filename="$(basename $1)"
filename="${filename%.*}"
f1="$filename.dot"
f2="$filename.png"
dot -Tpng "$f1" -o "$f2"