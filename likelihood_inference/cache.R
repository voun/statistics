
BLOCK_SIZE = c(1000, 2500, 3500, 4000, 4500, 5000, 6000, 7000, 75000, 10000, 15000, 20000, 25000, 30000, 40000, 50000, 60000)
miss_rate = c(0, 0, 0.1, 0.6, 1.1, 1.6, 1.1, 0.8, 0.8, 1.0, 2.0, 2.3, 2.3, 2.3, 2.4, 2.4, 2.4)
time = c(4.623, 3.45, 3.1, 3, 3, 3, 2.65, 2.7, 2.63, 2.6, 2.55, 2.55, 2.54, 2.535, 2.533, 2.53, 2.533)

plot(BLOCK_SIZE, miss_rate, type="p", col="blue", ylim = c(0,5), xlim = c(0,60000), ylab="seconds and %")
lines(BLOCK_SIZE, time, type="p", col="red")
legend("topright", legend=c("miss rate", "time"), col=c("blue", "red"), lw=c(1,1))



##Vi ser att när block_size <= 2500 får allt plats i cachen och miss raten är 0%
## detta är för att (32*1024-4*3645)/6 = 3031. Men med en sån liten block size kommer vi behöva göra betydligt 
## fler inläsningar och forka joina flera gånger som har en overhead. Därför kommer tiden var stor.
## Notera att cache line 64 bytes och vi använder shorts som är 2 bytes så om vi bara har våra arrayer
## så att elementen ligger efter varandra i minnet kommer vi aldrig över 1/32 = 3% miss rate 
## (i verkligheten verkar det faktiskt vara 2.4%). Så när vi har denna som miss rate utnyttjar vi inte blocking 
## överhuvudtaget. När block_size större än vad som ryms i cachen kommer vi fortfarande kunna cache en del
## tex block size 10000 så 1% miss rate. Detta är för att den inte är fully associative och kommer kunna
## komma ihåg en del "gammal" data. Nu utnyttjar vi block iteration lite i alla fall. För block size >= 15000
## kommer miss raten vara 2.4% och vi utnyttjar inte blocking alls och tiden kommer inte öka med block size längre.

## Anledningen till att vi har en peak kring block size = 5000 kan vara att vi flera gång läser
## cache lines från samma mängd och kommer då inte fylla upp cachen







#läs in allting och sen kolla cachen ta inte heller med parsing och kolla bara beräkna mellan blocken
#jämför tid med blocksize och cache miss rate. Förklara!!
#chunk size på static? borde välja tex block_size/n_threads för annars står någon och väntar i slutet
# varför inte bara ta dynamic då?? hålla koll på grejjer....
#prata lite om falshe sharing och vad som händer om ta dynamic istället för static när parsear.
#Räkna L1-cachen och hitta optimal block_size. Vad händer om tar riktigt små block_size tex 50,100,150?
#kolla när varierar med start --> slut och sedan slut-->start.
#gör något med perf????




