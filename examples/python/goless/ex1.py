import goless, time

channels = []
channels.append(goless.chan())
channels.append(goless.chan())
channels.append(goless.chan())

def makeWorker(params):
  name = params['name']
  def func1():
      print("{}[{}] waiting for val".format(name, params['val']))
      case, val = goless.select([goless.rcase(params['cin'])])
      print("{}[{}] got case={}, val={}".format(name, params['val'], case, val))
      time.sleep(params['sleep'])
      print("{}[{}] sending to cout...".format(name, params['val']))
      params['cout'].send("(" + name + ")=" + val + ";" + params['val'])
  return func1

funcs = [makeWorker({
  'name': 'workerA',
  'cin':  channels[0],
  'cout': channels[1],
  'val':  'one',
  'sleep': 1
  }), 
  makeWorker({
  'name': 'workerB',
  'cin':  channels[1],
  'cout': channels[2],
  'val':  'two',
  'sleep': 1
    })]

for f in funcs:
  goless.go(f)

channels[0].send('(main)=inval')

for i in range(1):
    case, val = goless.select([goless.rcase(channels[2])])
    print("result={}".format(val))

