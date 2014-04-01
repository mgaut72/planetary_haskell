default:
	ghc --make -O2 Main

profile:
	ghc --make -O2 Main -prof -auto-all -caf-all -fforce-recomp

clean:
	rm *.hi *.o SolarSystem/*.hi SolarSystem/*.o Visualization/out.txt *.prof
