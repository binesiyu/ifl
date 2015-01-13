for var in Contributions gm lambda language pargm preface template tim utils;
  do 
  echo $var
  cat $var.src | grep '^G*H*[1-9\-]*>' | sed 's/^G*H*> \{0,1\}//g' | sed 's/^G*H*\([1-9\-]\{1,\}\)> \{0,1\}/{-exs_\1-}/g' >$var.hs
done;
