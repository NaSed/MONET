py -2.7 -i C:\Users\sedaghat\Dropbox\MongooseGUI\ModelProcessing.py
s = shelve.open('C:\Users\sedaghat\Dropbox\MongooseGUI\ExampleModels\EC6Model')
model = s['EC6']
M = model.Matrix
m = len(M)
string  = ''
string += ';'.join([','.join([str(x) for x in M[i]]) for i in range(m)])
string += ';'
f = open('C:\Users\sedaghat\Dropbox (Personal)\MONET_MetabolicNetworks\Data\RealMetabolicNetworks\EC6.txt','w')
f.write(string)
f.close()

irr=model.findIrreversibleReactions()
m = len(irr)
string  = str(irr)
f = open("C:\Users\sedaghat\Dropbox (Personal)\MONET_MetabolicNetworks\Data\RealMetabolicNetworks\EC6_irr.txt",'w')
f.write(string)
f.close()
quit()
