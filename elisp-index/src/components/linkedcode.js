import PropTypes from "prop-types";
import React from "react";

const RefLink = ({ call, fnNameToFile }) => {
  let url = "#";
  let className = "has-text-danger";
  if (call.namespace == "macro") {
    className = "has-text-success";
  } else if (fnNameToFile[call.name]) {
    url = "/file/" + fnNameToFile[call.name];
    className = "";
  }
  // TODO: use a proper Link component.
  return (
    <a className={className} key={call.name + call.start} href={url}>
      {call.name}
    </a>
  );
};

function butLast(arr) {
  return arr.slice(0, arr.length - 1);
}

function last(arr) {
  return arr[arr.length - 1];
}

const LinkedCode = ({ source, calls, fnNameToFile }) => {
  let lines = [];
  let currentLine = [];

  function appendText(text) {
    let textLines = text.split("\n");

    if (textLines.length == 1) {
      // Text is not a whole line, add it to current line.
      currentLine.push(text);
    } else {
      // Text is several lines, add them to lines directly.
      lines.push(currentLine);
      currentLine = [];
      butLast(textLines).forEach(prefixLine => {
        lines.push(<code>{prefixLine + "\n"}</code>);
      });
      currentLine.push(last(textLines));
    }
  }

  let i = 0;
  calls.forEach(call => {
    if (call.start > i) {
      let prefix = source.substring(i, call.start);
      appendText(prefix);
    }

    currentLine.push(<RefLink call={call} fnNameToFile={fnNameToFile} />);
    i = call.end;
  });
  if (i < source.length) {
    appendText(source.substring(i));
  }

  if (currentLine.length > 0) {
    lines.push(
      <code>
        {currentLine}
        {"\n"}
      </code>
    );
  }

  return (
    <div>
      <pre>{lines}</pre>
    </div>
  );
};

LinkedCode.propTypes = {
  source: PropTypes.string
};

export default LinkedCode;
