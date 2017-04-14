# cl-maxseg, a high-performance Common Lisp implementation of maximum probability segmentation 
## About
This is an implementation of maximum probability segmentation algorithm. Benefitting the power of lisp language and the fascinating Kneser-Ney language model algorithm, the software gains the features below:

1. Fast. Built with SBCL with multicore support, it achieves speed of 1.5MB/s with our test data.
2. Easy-to-use. You can just use our pre-built binary release or choose to train your own model. The process is trivial and fast.
3. Cross-platform. We support various CL implementations such as ECL, SBCL, CCL and CMUCL (oops CLISP failed to test, sorry.) and you can build the binary release on either Windows or Unix-like.
4. Robust. Well now the software is pretty stable and bug-free. (hopefully.)

We provide both source code (MIT license) and binary release. 

## Build & Run
### Dependency 
- Quicklisp, of course.
- cl-kn, a Common-Lisp implementation of Kneser-Ney algorithm. See [https://github.com/niwtr/cl-kn](here).
- Excalibur, a Common Lisp conveniency lib. See [https://github.com/niwtr/Excalibur](here).
- For multicore support, package lparallel.

### Build 
In Unix-like, like Mac OSX and Linux, pick up Clozure CL and run 
```bash
ccl --load main.lisp
```
That's all. 
### Run 
Several binary releases are provided in the repo. Just double-click and 
## Permission & Acknowledges 
You are free to use this software under MIT license. 
Special thanks to Li, my girlfriend, you lightened my life :)


