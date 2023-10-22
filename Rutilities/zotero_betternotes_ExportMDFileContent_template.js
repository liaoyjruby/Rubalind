let lines = mdContent.split("\n");
function isListLine(line) {
    let lineTrim = /^(?:\*|-|\+|[0-9]+\.)(?= )/.test(line.trim())
    return lineTrim;
}
let filteredLines = [];
for (let i = 0; i < lines.length; i++) {
    let currentLine = lines[i];
    if (i > 0 && i < lines.length - 1) {
        // Not first or last line. Check empty lines between item list lines
        let previousLine = lines[i-1];
        let nextLine = lines[i+1];
        if (isListLine(previousLine)
            && (isListLine(nextLine))
            && currentLine.trim().length === 0) {
                // Skip empty line
                continue;
        }
    }
    filteredLines.push(currentLine);
}
// Do latex & semicolon empty line removal
function isLatexLine(line) { // start and end with '$'
    return /^\$.+\$$/.test(line.trim())
}
function hasLatexLine(line) { // has 2 '$'
    return /\$.+\$$/.test(line.trim())
}
function isColonLine(line) {
    return line.startsWith(":")
}
function isTitleLine(line) {
    return line.startsWith("#")
}
let mergeLines = [];
for (let i = 0; i < filteredLines.length; i++) {
    let currentLine = filteredLines[i];
    if (i > 0 && i < filteredLines.length - 1) {
        // Not first or last line. Check empty lines between item list lines
        let previousLine = filteredLines[i-1];
        let nextLine = filteredLines[i+1];
        if ((hasLatexLine(nextLine) || hasLatexLine(previousLine) || isColonLine(nextLine))
            && currentLine.trim().length === 0) {
            continue;
        }
    }
    mergeLines.push(currentLine);
}
// Do line merging
let catLines = [];
let mergeLine = "";
for (let i = 0; i < mergeLines.length; i++) {
    let currentLine = mergeLines[i];
    if (i > 0 && i < mergeLines.length - 1) {
        let previousLine = mergeLines[i-1];
        let nextLine = mergeLines[i+1];
        if(isLatexLine(currentLine) && !isListLine(nextLine)) { // will be on its own line
            mergeLine = previousLine + " " + currentLine.trim() + " " + nextLine.trim()
            mergeLines.splice(i, 2)
            i -= 1
            mergeLines[i] = mergeLine
            catLines.pop()
            catLines.push(mergeLine)
            continue;
        }
    }
    catLines.push(currentLine);
}
// Handle latex as list line
let catLines2 = [];
let mergeLine2 = "";
for (let i = 0; i < catLines.length; i++) {
    let currentLine = catLines[i];
    if (i > 0 && i < catLines.length - 1) {
        let nextLine = catLines[i+1];

        if(isLatexLine(currentLine.trim().substring(1).trim()) && !isListLine(nextLine) && !isTitleLine(nextLine)) {
            mergeLine2 = currentLine + " " + nextLine.trim()
            catLines.splice(i+1, 1)
            catLines[i] = mergeLine
            catLines2.push(mergeLine2)
            continue;
        }
    }
    catLines2.push(currentLine);
}
// Handle latex after list lines
let catLines3 = [];
let mergeLine3 = "";
for (let i = 0; i < catLines2.length; i++) {
    let currentLine = catLines2[i];
    if (i > 0 && i < catLines2.length - 1) {
        let previousLine = catLines2[i-1];
        if(isLatexLine(currentLine.trim()) && isListLine(previousLine)) {
            mergeLine3 = previousLine + " " + currentLine.trim()
            catLines2.splice(i, 1)
            i -= 1
            catLines2[i] = mergeLine3
            catLines3.pop()
            catLines3.push(mergeLine3)
            continue;
        }
    }
    catLines3.push(currentLine);
}
return catLines3.join("\n");