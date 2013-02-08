for i in de eo nl pt_BR fr; do
	msgfmt -v $i -o `basename $i .po`.mo
done
mv *.mo ..
