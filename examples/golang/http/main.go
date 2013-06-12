package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"net/http"
	"time"
)

type Result struct {
	Url     string
	Stime   time.Time
	Etime   time.Time
	Elapsed time.Duration
	Status  bool
	Body    string
	Err     error
}

func (self *Result) String() string {
	return fmt.Sprintf("Result{Url:%s; Elapsed:%q; Status:%q, Body:%d/%s, Err:%q}",
		self.Url,
		self.Elapsed,
		self.Status,
		len(self.Body),
		string(self.Body[0:10]))
}

func main() {
	numThreads := 2
	numRequests := 10
	flag.IntVar(&numThreads, "threads", 2, "Number of goroutines to run")
	flag.IntVar(&numRequests, "requests", 10, "Total requests to make to the URL.")
	flag.Parse()

	currency := make(chan int)
	results := make(chan *Result)

	if len(flag.Args()) == 0 {
		panic("You must supply a URL")
	}

	url := flag.Args()[0]

	for ii := 0; ii < numThreads; ii++ {
		fmt.Printf("creating new routine:%d of %d\n", 1+ii, numThreads)
		go func(url string, in chan int, out chan *Result) {
			for nn := range in {
				fmt.Printf("[#%d] Make request : %s\n", nn, url)
				res := &Result{
					Url:   url,
					Stime: time.Now(),
				}
				resp, err := http.Get(url)
				//fmt.Printf("Made request, resp is %q chars\n", resp)
				if err == nil {
					defer resp.Body.Close()
					body, err := ioutil.ReadAll(resp.Body)
					fmt.Printf("[#%d] Made request, body is %d chars\n", nn, len(body))
					if err == nil {
						res.Body = string(body)
					}
					res.Err = err
				} else {
					fmt.Printf("[#%d] Made request, err is: %q\n", nn, err)
					res.Err = err
				}
				res.Etime = time.Now()
				res.Elapsed = res.Etime.Sub(res.Stime)
				res.Status = res.Err != nil
				out <- res
			}
			fmt.Printf("requester done (channel empty)\n")
		}(url, currency, results)
	}

	go func() {
		fmt.Printf("putting in the currency\n")
		for ii := 0; ii < numRequests; ii++ {
			currency <- ii
		}
		close(currency)
	}()

	fmt.Printf("consuming the results\n")

	// NB: how do we close the results channel so we don't end up watiing fomr
	// something that will never come? (eg if a request fails)
	totalElapsed := 0.0
	totalRequests := 1.0
	bodyBytes := 0
	for ii := 0; ii < numRequests; ii++ {
		res := <-results
		totalRequests += 1.0
		totalElapsed += float64(res.Elapsed.Nanoseconds()) / (1000.0 * 1000.0)
		bodyBytes += len(res.Body)
		fmt.Printf("result: %.0f requested, %3.2fms avg response, %.2f bytes avg\n",
			totalRequests,
			totalElapsed/totalRequests,
			float64(bodyBytes)/totalRequests)
		// fmt.Printf("result: %q\n", res)
	}

	close(results)
}
