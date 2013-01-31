import re
from collections import deque
from profiler import profile, print_prof_data  

def unrecognized_token(s):
    raise Exception("The string \'%(s)s\' cannot be tokenized." % locals())

# this is the tokenizer for basic SOCP problems    
scanner=re.Scanner([
    (r"#.*",                    None ), # gobble comments
    (r"variable ",               lambda scanner,token:("VARIABLE", token) ),
    (r"parameter ",              lambda scanner,token:("PARAMETER", token) ),
    (r"vector",                 lambda scanner,token:("VECTOR", token) ),
    (r"scalar",                 lambda scanner,token:("SCALAR", token) ),
    (r"matrix",                 lambda scanner,token:("MATRIX", token) ),
    (r"positive|nonnegative",   lambda scanner,token:("POSITIVE", token) ),
    (r"negative|nonpositive",   lambda scanner,token:("NEGATIVE", token) ),
    (r"minimize ",               lambda scanner,token:("MINIMIZE", token) ),
    (r"maximize ",               lambda scanner,token:("MAXIMIZE", token) ),
    (r"find ",                   lambda scanner,token:("FIND", token) ),
    (r"subject to",             lambda scanner,token:("SUBJECT_TO", token) ),
    (r"==",                     lambda scanner,token:("EQ", token) ),
    (r"<=",                     lambda scanner,token:("LEQ", token) ),
    (r">=",                     lambda scanner,token:("GEQ", token) ),
    (r"0+|0+\.0*",              lambda scanner,token:("ZEROS", float(token)) ),
    (r"ones",                   lambda scanner,token:("ONES", 1.0) ),
    # abs and norm are built in for the second-order cones
    (r"abs",                    lambda scanner,token:("ABS", token) ),
    (r"norm",                   lambda scanner,token:("NORM", token) ),
    (r"\d+\.\d*",               lambda scanner,token:("CONSTANT", float(token)) ),
    (r"\d+",                    lambda scanner,token:("CONSTANT", float(token)) ),
    (r"\(",                     lambda scanner,token:("LPAREN", token) ),
    (r"\)",                     lambda scanner,token:("RPAREN", token) ),
    (r"[a-zA-Z_]+",  	        lambda scanner,token:("IDENTIFIER", token) ),
    (r"\+",                     lambda scanner,token:("PLUS_OP", token) ),
    (r"\-",                     lambda scanner,token:("MINUS_OP", token) ),
    (r"\*",                     lambda scanner,token:("MULT_OP", token) ),
    (r"\s+",                    None), # None == skip token.
    (r".",                      lambda scanner,token:unrecognized_token(token))
])



class ScoopParser: 
    """A simple parser for the SCOOP language"""
    
    # counter for new variables introduced
    varcount = 0
    # dictionary for symbol table
    symtable = {}
    # parser STATE
    state = ""
    # current line
    line = ""
    
    def lex(self, s):
        """Tokenizes the input string 's'. Uses the hidden re.Scanner function."""
        return scanner.scan(s)
    
    # TODO: add a lexer that checks the grammar and converts expressions to
    # RPN
    def parse(self,toks):
        """Parses the tokenized list. Could use LR parsing, but SCOOP is simple
        enough that expressions are parsed with a shunting-yard algorithm.
        Everything else is just simple keywords.
        """
        
        # four types of statements:
        #    variable declarations
        #    parameter declarations
        #    minimize objective
        #      ("subject to")
        #    constraints
        actions = { 
            'VARIABLE': self.parse_variable, 
            'PARAMETER': self.parse_parameter,
            'SUBJECT_TO': self.parse_subject_to
        }
        
        # if not one of four listed actions, the default is to attempt to
        # parse a constraint
        actions.get(toks[0][0], lambda x: None)(toks)
        
        # if toks[0][0] is 'VARIABLE':
        #     self.parse_variable(toks)
        # elif toks[0][0] is 'PARAMETER':
        #     toks.popleft()
        #     self.parse_parameter(toks)
        # elif
        # else:
        #     print ' '.join( map(lambda x:x[0], toks) )
        
    def display(self,toks):
        print ' '.join( map(lambda x:str(x[1]), toks) )
   
    @profile
    def run(self,s):
        self.line = s
        result, remainder = self.lex(s)
        if result:
            if not remainder:
                self.parse(deque(result))
                self.display(result)
                print '\n'
            else:
                raise Exception("Unknown parse error.")
    
    # parsing functions to follow
    def parse_variable(self,toks):
        """variable IDENTIFIER VECTOR|SCALAR (POSITIVE|NEGATIVE)"""
        self.state = ""
        s = self.line
        toks.popleft()  # already matched variable keyword
        
        t, v = toks.popleft()
        if t is not "IDENTIFIER":
            raise SyntaxError("\"%(s)s\"\n\tExpected an identifier, but got %(t)s with value %(v)s instead." % locals())
            
        t, tmp  = toks.popleft()
        if t is not "VECTOR" and t is not "SCALAR":
            raise SyntaxError("\"%(s)s\"\n\tExpected a VECTOR or SCALAR shape, but got %(tmp)s instead." % locals())
        
        if v in self.symtable:
            raise Exception("\"%(s)s\"\n\tThe name %(v)s is already in use for a parameter / variable." % locals())
        else:
            self.symtable[v] = t
        
        if toks:
            t, tmp = toks.popleft()
            if t is not "POSITIVE" and t is not "NEGATIVE":
                raise SyntaxError("\"%(s)s\"\n\tExpected a POSITIVE or NEGATIVE sign, but got %(tmp)s instead." % locals())
            self.symtable[v] += " " + t
            
            # if any remaining
            if toks:
                t, tmp = toks.popleft()
                raise SyntaxError("\"%(s)s\"\n\tUnexpected ending with %(t)s %(tmp)s." % locals())
    
    def parse_parameter(self,toks):
        """parameter IDENTIFIER VECTOR|SCALAR (POSITIVE|NEGATIVE)"""
        self.state = ""
        s = self.line
        toks.popleft()  # already matched parameter keyword
        
        t, v = toks.popleft()
        if t is not "IDENTIFIER":
            raise SyntaxError("\"%(s)s\"\n\tExpected an identifier, but got %(t)s with value %(v)s instead." % locals())
            
        t, tmp  = toks.popleft()
        if t is not "VECTOR" and t is not "SCALAR" and t is not "MATRIX":
            raise SyntaxError("\"%(s)s\"\n\tExpected a VECTOR, SCALAR, or MATRIX shape, but got %(tmp)s instead." % locals())
        
        if v in self.symtable:
            raise Exception("\"%(s)s\"\n\tThe name %(v)s is already in use for a parameter / variable." % locals())
        else:
            self.symtable[v] = t
        
        if toks:
            t, tmp = toks.popleft()
            if t is not "POSITIVE" and t is not "NEGATIVE":
                raise SyntaxError("\"%(s)s\"\n\tExpected a POSITIVE or NEGATIVE sign, but got %(tmp)s instead." % locals())
            self.symtable[v] += " " + t
            
            # if any remaining
            if toks:
                t, tmp = toks.popleft()
                raise SyntaxError("\"%(s)s\"\n\tUnexpected ending with %(t)s %(tmp)s." % locals())
    
    def parse_subject_to(self,toks):
        toks.popleft()
        if self.state:
            self.state = ""
        else:
            raise Exception("Cannot use optional \"subject to\" keyword without corresponding minimize, maximize, or find.")


# if the lang were embedded in python, could do some crazy stuff
# problem is getting variable names from the symbol table
if __name__ == '__main__':
    print "hello"
    p = ScoopParser()
    print p.varcount
    p.varcount += 1
    print p.varcount
    
    # i can parse scoop line by line
    # each "file" is just a code block, within the block i just require
    # that you had the thing defined before
    
    map (p.run, ["# this entire line is a comment!",
     "variable x vector positive # hello, this is way too cool blah",
     "variable _x vector",
     "parameter A matrix positive",
     "minimize norm(A*x - b)",
     "norm(x) <= 3",
     #"subject to",
     "a >= 0",
     "abs(x) <= t",
     "sqrt(3*x-y) >= abs(z)"])
    
    print p.symtable
    
    print_prof_data()
