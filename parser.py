import re 

chunks = []

moveRe = r'.*move\((?P<action>[a-zA-Z\_]+)\(.*'
succRe = r':\s\(\d+\)\sstack\(.+\)'
addedActionsRe = r'stack\([a-zA-Z]+\(.*\), \[.*\], \[(?P<newActions>.*)\])'
newPlanRe = r'Call: \(\d+\) plan\(\[(?P<newState>[^\[\]]*)\],\s\[.*\],\s\[.*\],\s\[.*\]\)'
checkConditionsRe = r'conditions_met\(\[(?P<toMeet>[^\[\]]*)\],\s\[(?P<state>[^\[\]]*)\].*'
condToMeetRe = r'(?P<actionName>[a-zA-Z]+)\([^\(\)]+\)'

def parseChunk(chunk):
    succ = False
    m = re.search(succRe, chunk)
    if m:
        succ = True

    actionName = ""
    m = re.match(moveRe, chunk)
    if m:
        actionName = m["action"]
    else:
        raise Exception("Name could not be parsed:\n", chunk)

    if succ:
        newState = ""
        m = re.search(newPlanRe, chunk)
        if m:
            newState = m['newState']
        else:
            raise Exception("Could not find newState:\n", chunk)

        print("\x1b[5;30;42m{} -> S -> [{}]\x1b[0m".format(actionName, newState))

    else:
        notMet = []
        toMeet = ""
        state = ""
        m = re.search(checkConditionsRe, chunk)
        if m:
            toMeet = m["toMeet"]
            state = m["state"]
        else: 
            raise Exception("Could not find conditions:\n", chunk)


        predInState = []
        for pred in re.finditer(condToMeetRe, state):
            predInState.append(pred["actionName"]+'(')

        for condTmp in re.finditer(condToMeetRe, toMeet):
            cond = condTmp["actionName"].split("(")[0]+'('
            if cond not in predInState:
                notMet.append(cond[:-1])
        
        if len(notMet) == 0:
            notMet = ["inposition"]


        missing = ""
        for i in notMet:
            missing += i+", "
        missing = missing[:-2]

        print("\x1b[0;30;41m{} -> F -> [{}]\x1b[0m".format(actionName, missing))


def readFile(filename):
    global chunks
    with open(filename) as file:
        newline = ""
        for line in file:
            m = re.match(moveRe, line)
            if m:
                chunks.append(newline)
                newline = ""
            newline+=line
        chunks.append(newline)

    chunks = chunks[1:]


def main():
    readFile("file.txt")
    for chunk in chunks:
        parseChunk(chunk)

if __name__ == "__main__":
    main()
