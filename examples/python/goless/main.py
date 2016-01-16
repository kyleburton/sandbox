import goless
import logging

def make_worker(workerfn, cin, cout, startWorker=True):
    def worker():
        while True:
            case, val = goless.select([goless.rcase(cin)])
            if val is None:
                print("cin returned None, is it closed?")
                cout.close()
                return
            print("f1: case={}, val={}".format(case, val))
            cout.send(workerfn(val))
    if startWorker:
        res = goless.go(worker)
        print("started worker: {} => {}".format(worker, res))
    return worker

def logme(val):
  print("logme: val={}".format(val))
  logging.info("logme: val={}".format(val))
  return "logme={}".format(val)

def run():
    channels = {
        'c1': goless.chan(0),
        'c2': goless.chan(0),
        'c3': goless.chan(0)
    }
    workers = [make_worker(logme, channels['c1'], channels['c2']), 
               make_worker(logme, channels['c2'], channels['c3'])]

    logging.info("run: defining send_messages...")
    print("run: defining send_messages...")
    def send_messages():
        for ii in range(10):
            print("sending val={} into channel['c1']={}", val, channels['c1'])
            val = "val:{}".format(ii)
            channels['c1'].send(val)

    res = goless.go(send_messages)
    print("called goless.go(send_messages) => {}".format(res))

    while True:
        cases = [goless.rcase(channels['c3']),
                 goless.dcase()]
        case, val = goless.select(cases)
        if case == cases[1]:
            print("hit default, perhaps things are closed? case={} val={}".format(case, val))
            break
        print("case={}; c3 => {}".format(case, val))
        print("closing channel c3")
        channels['c3'].close()
        print("exiting")

if __name__ == '__main__':
    run()
