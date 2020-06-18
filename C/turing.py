#!/usr/bin/python

# #################################################################
__license__= '''This module (version Mar/2015),
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License.'''
__author__ = "NelsonManohar"
__date__   = "Mar2015"
__doc__    = '''a pure python turing machine tape reader handling extended
finite state machine automata for the printable ascii character set, 
assumptions given: tape is not infinite nor very long
assumption: tape initial indexing starts at 0'''
print '-' * 80
print __doc__
print '-' * 80
print __license__
print '-' * 80
# #################################################################


# #################################################################
from collections import defaultdict
import time
import argparse
import sys
import string
# #################################################################


# #################################################################
class TAPE_READER_CLASS(object):
    # ###########################################################
    def __init__(self, tapefile, maxiter=0, debug=1):
        self.DEBUG = debug
        self.TAPEFILE = tapefile
        self.MAXITER = maxiter
        self.NITER = 0
        self.TAPE = None
        self.ACTION_TABLE = defaultdict(list)
        self.TAPE_STATE = dict(zip(('contents','blank','position','ps_state','hs_state', 'direction'),
                                    (None,      '-',    None,      None,      None,      None)))
        self.DONE = False
        self.MATCHING_STATE = []
        #self.load_tape_header()
        #self.load_action_table()
        #self.print_action_table()
        #self.main()
        return
    # ###########################################################

    # ###########################################################
    def INFO(self, msg, val):
        print '-' * 80
        print "INFO   : [%s] [%s]" % (msg, val)
        return val
    # ###########################################################

    # ###########################################################
    def ERROR(self, msg, val):
        print "ERROR  : [%s] [%s]" % (msg, val)
        self.done(-1)
        return val
    # ###########################################################

    # ###########################################################
    def WARNING(self, msg, val ):
        print "WARNING: [%s] [%s]" % (msg, val)
        return val
    # ###########################################################

    # ###########################################################
    def print_action_table(self):
        print '-' * 80
        print "[%s] [%s] THEN write char [%s], update dir to [%s];\t set next state -> [%s]" % ( "PS", "C", "W", "D", "NS" ) 
        print '-' * 80
        akeys = sorted(self.ACTION_TABLE.keys())
        for ps in akeys:
            for actions in self.ACTION_TABLE[ps]:
                print "[%s] [%s]\t THEN write char [%s], update dir to [%s];\t set next state -> [%s]" % tuple(actions)
        print '-' * 80
    # ###########################################################

    # ###########################################################
    def load_action_table(self):
        '''
        loads an tape reader automata action table, where each row of an action table 
        automata specification is expected to match this exacting format
                <State index> <If character> <Write> <Direction> <New state index>
        '''
        self.ACTION_TABLE  = defaultdict(list)
        try:
            with open(self.TAPEFILE) as tape:
                tape.readline()
                tape.readline()
                tape.readline()
                tape.readline()
                done = False
                while not done:
                    try:
                        line = tape.readline().strip()
                        if not line: break
                        (ps, ifchar, writechar, direction, ns) = line.split(' ')
                        ps, direction, ns = int(ps), int(direction), int(ns)
                    except:
                        done = True
                        self.ERROR("ACTION TABLE LINE IS WRONG", line)
                    self.ACTION_TABLE[ps].append([ps, ifchar, writechar, direction, ns])
    
                if not self.ACTION_TABLE:
                    self.ERROR("ACTION TABLE SPECIFICATION MISSING", self.TAPEFILE)
        except:
            self.ERROR("ACTION TABLE SPECIFICATION IS WRONG", self.TAPEFILE)
        return self.ACTION_TABLE
    # ###########################################################
    
    # ###########################################################
    def load_tape_header(self):
        ''' Expects this format:
        <Starting contents of tape>\n
        <Index into above string determining start position of head>\n
        <Start state index (integer)>\n
        <Halting state index (integer)>\n
        <Action table>
        '''
        try:
            with open(self.TAPEFILE) as tape:
                chars = tape.readline().strip()
                pos = tape.readline().strip()
                ps = tape.readline().strip()
                hs = tape.readline().strip()
                if self.DEBUG:
                    print "%s\n, %s %s %s " % (chars, pos, ps, hs)
        except:
            self.ERROR("TAPE HEADER FORMAT IS WRONG", self.TAPEFILE)

        self.TAPE_STATE['contents'] = chars
        self.TAPE_STATE['tape_out'] = []
        try:
            self.TAPE_STATE['position'] = int(pos)
            self.TAPE_STATE['ps_state'] = int(ps)
            self.TAPE_STATE['hs_state'] = int(hs)
        except:
            self.ERROR("TAPE HEADER CONTAINS INVALID STATE/POSITION, INTEGER EXPECTED", (pos,ps,hs))

        if not all(x in string.printable for x in chars):
            self.ERROR("TAPE CONTAINS NON PRINTABLE CHARACTERS", chars)

        return self.TAPE_STATE
    # ###########################################################

    # ###########################################################
    def readchar(self):
        '''Assumes tape head is already positioned and returns proper character'''
        ifchar = None
        self.NITER += 1
        try:
            pos = self.TAPE_STATE['position']
            if pos < 0 or pos > len(self.TAPE_STATE['contents']):
                self.ERROR("INVALID TAPE POSITION, REACHED OUTSIDE TAPE BOUNDARIES", pos)
            ifchar = self.TAPE_STATE['contents'][int(pos)]
            print "[ITERATION: %s] AT POS[%s], READ [%s]" % (self.NITER, pos, ifchar)
        except:
            self.ERROR("INVALID TAPE POSITION", pos)
        return ifchar
    # ###########################################################
    
    # ###########################################################
    def next_state(self, char):
        ps = self.TAPE_STATE['ps_state']
        states = self.ACTION_TABLE[ps]

        matching_state = ()
        for state in states:
            (ps, ifchar, writechar, direction, ns) = state
            if char == ifchar:
                matching_state = state[:]
                break
            if ifchar == "*":
                writechar = ""      # if matching state is *, then write NOTHING as specified
                matching_state = (ps, ifchar, writechar, direction, ns)
                break

        if not matching_state:
            self.ERROR("ACTION TABLE LACKS RULE FOR", "PS=%s IFCHAR=[%s])" % (ps, char))

        self.MATCHING_STATE = matching_state[:]
        return self.MATCHING_STATE
    # ###########################################################
    
    # ###########################################################
    def update_state(self, char, action_state):
        (ps, ifchar, writechar, direction, ns) = action_state
        if self.DEBUG:
            print "(%s:(%s->%s))->(%s,%s)" % (ps, char, ifchar, writechar, ns)
        self.TAPE_STATE['direction'] = int(direction)
        self.TAPE_STATE['position'] = int(self.TAPE_STATE['position']) + int(direction)
        self.TAPE_STATE['ps_state'] = ns
        self.TAPE_STATE['tape_out'].append((char,writechar))
        return
    # ###########################################################

    # ###########################################################
    def print_mapping(self):
        for i, pair in enumerate(self.TAPE_STATE['tape_out']):
            print "%6s: %s --> %s" % (i, pair[0], pair[1])
        return self.TAPE_STATE['tape_out']
    # ###########################################################

    # ###########################################################
    def halting_tests(self):
        if self.TAPE_STATE['ps_state'] not in self.ACTION_TABLE.keys():
            self.INFO("TAPE READER FOUND AT UNDEFINED ACTION STATE\n", self.TAPE_STATE)
            if self.TAPE_STATE['ps_state'] == self.TAPE_STATE['hs_state']:
                self.INFO("TAPE READER REACHED HALTING STATE\n", self.TAPE_STATE)
            self.DONE = True
            self.done(-1)
        if self.TAPE_STATE['ps_state'] == self.TAPE_STATE['hs_state']:
            self.INFO("TAPE READER REACHED HALTING STATE\n", self.TAPE_STATE)
            self.DONE = True
            self.done(0)
        if self.MAXITER:
            if self.NITER > self.MAXITER:
                self.INFO("MAXIMUM NUMBER OF ITERATIONS REACHED BEFORE HALTING STATE\n", self.TAPE_STATE)
                self.DONE = True
                self.done(1)
        return
    # ###########################################################

    # ###########################################################
    def done(self, how):
        print '-' * 80
        tapeInput = "".join([x for x in self.TAPE_STATE['contents']])
        tapeRead = "".join([x[0] for x in self.TAPE_STATE['tape_out']])
        tapeOutput = "".join([x[1] for x in self.TAPE_STATE['tape_out']])
        print "TAPE CONTENTS: ", tapeInput
        print "TAPE TRAVERSAL:", tapeRead
        print "OUTPUT STREAM: ", tapeOutput
        print '-' * 80
        if how != 0:
            sys.exit(how)
        return (tapeInput, tapeRead, tapeOutput)
    # ###########################################################

    # ###########################################################
    def main(self): 
        while not self.DONE:
            self.halting_tests()
            if not self.DONE:
                char = self.readchar()
                self.MATCHING_STATE = self.next_state(char)
                self.update_state(char, self.MATCHING_STATE)
                self.print_mapping()
        return self.TAPE_STATE


    # ###########################################################
# #################################################################

    
# #################################################################
if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog='turing.py',
             description='This program operates a virtual tape reader wrt an extended finite state machine automata specification.')
    parser.add_argument("--tapefile",
             default='tapefile.txt', help='filepath to file containing the lines to be sampled.')
    parser.add_argument("--maxiter", type=float,
             default=0, help='if non zero, a max. number of traversals over the tape, for handling infinite loops.')
    args = parser.parse_args()
    parser.print_help()

    print >>sys.stderr, '-' * 80
    print >>sys.stderr, "tape file to scan:", args.tapefile
    print >>sys.stderr, '-' * 80

    tapeReader = TAPE_READER_CLASS(args.tapefile, maxiter=args.maxiter)
    tapeReader.load_tape_header()
    tapeReader.load_action_table()
    tapeReader.print_action_table()
    tapeReader.main()



