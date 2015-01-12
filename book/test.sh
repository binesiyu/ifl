for var in Contributions gm lambda language pargm preface template tim utils;
  do 
  echo $var
  cat $var.src | grep '^G*H*>' | sed 's/^G*H*> \{0,1\}//g' >$var.hs
done;
