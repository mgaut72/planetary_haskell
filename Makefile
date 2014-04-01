default:
	ghc --make -O2 Main

clean:
	rm *.hi *.o SolarSystem/*.hi SolarSystem/*.o Visualization/out.txt
