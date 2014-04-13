From Zero to Computer: Week #0
==============================

Some time ago I decided that, in order to truly understand a computer, I would have to design one... from scratch. Don't get me wrong; most of my professional life is kept at a very high-level of abstraction and I actively evangelize people that way. I program in Scala. I teach Agile. Part of my daily job is doing (and thinking about) machine learning. I once studied typed lambda-calculus, got my dose of theorem-proving in Coq, and proceded to teach formal methods in software engineering. I understand the phenomena of computation as being decoupled from the physical media were it is performed, up to the point that I believe in hard-AI.

And yet, somehow, the low-level engineering details of electronics fascinate me...

So the time is due. Here's my new hobby goal: no further than the 31th of December 2014, I will have done the following:

1. Designed a logic circuit simulator in Scala
2. Proceeded to implement the fundamentals of electronic systems, such as logic gates, flip-flops, muxes/demuxes, etc.
3. Design on top of that a computer system, where I'll do my best effort to be non-von-Neumann (though this is not guaranteed)
4. Implement a basic compiler for that computer in Scala
5. Proceed to bootstrap that compiler (i.e., make the compiler able to compile itself)
6. Implement a basic operating system to run on that computer
7. Implement an application for that OS that acts as a real-time monitor of the code being executed
8. Physically build this system with VGA output from common IC's

Since I have little more than 8 months until the 31th of December, this means that each topic above must be completed in a single month, being allowed to use *only* my spare time. This means that I cannot *ever* touch the source-code before 8PM or during weekends, holidays and vacations. And I'll blog everything while I go.

Readers beware: I've never attempted anything similar, and I'll learn as I go along. Do not use assume anything from this blog posts other than that I might probably be doing it the wrong way.

The Basics
----------

The most fundamental thing one should remember is that we'll be dealing with circuits that regard their logic as binary: there's only two possible states of a signal (carried by a line), and those are usually called HIGH and LOW (1 and 0, true or false, etc.). In this logic, the law of excluded middle is axiomatic; a line is either carrying an HIGH state or a LOW state. Nothing more is possible. But in practice, things get a little muddier. Signals are usually interpreted by probing the voltage they make with ground. If the circuit is 5V-based, then a value near 5V is regarded as HIGH, and something near 0V is regarded as LOW. What happens around 2.5V is out-of-scope of this post.

Wikipedia says (http://en.wikipedia.org/wiki/Combinational_logic) there are two types of circuits: combinatorial and sequential. Personally, the word "combinatorial" seems very awkward to describe what they do, so I find useful to know that they are also called "time-independent". Hence:

1. Combinatorial logic is implemented disregarding any component of time. They remind me of pure functions; given the same arguments, they return the same answer. No state whatsoever. Not timing whatsoever. Probably the most basic example of a combinatorial circuit is the "identity" circuit also known as a "wire" (yes, I just made that up, but somehow it makes sense). Whatever goes in, also goes out. The second most basic is a NOT gate. If there's an HIGH going in, there's a LOW going out. If there's a LOW going in, there's an HIGH going out. Then comes the usual AND, OR, NAND, NOR and XOR gates; they accept two inputs and provide a single output.
2. Sequential logic involves state; wikipedia describes these circuits as depending "not only on the present value of its input signals but on the past history of its inputs". I'm not sure how to interpret this yet, but for now I'll think of these as objects instead of pure functions, with an internal "private" state. There's a second distinction between "synchronous" and "asynchronous" sequential logic, but that involves us understanding the concept of a clock. The most basic element wikipedia gives me is the "flip-flop": something that if I send an HIGH in a line called SET, it will keep outputting an HIGH... until I send an HIGH in a line called RESET, where it will proceed to output a LOW henceforth.

The Simulator
-------------

Everything above seems nice, but I have a problem on the back of my mind concerning combinatorial circuits. It seems that, no matter how fast each circuit may be, it can't be faster than light. Signals will take time to be propagated through a line, and gates will take their time to react to signals. Abstractly, I think this means that there might be an order of evaluation going on. But without too much premature concern, it seems a safe bet to imagine that the lines and gates will act as nodes and edges of a graph. It also seems a safe bet to understand it as a directed graph. I'm not sure yet if it is "acyclic" due to the way I've seen flip-flops implemented.
