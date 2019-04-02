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

const LinkedCode = ({ source, calls, fnNameToFile }) => {
  let parts = [];
  let i = 0;
  calls.forEach(call => {
    if (call.start > i) {
      parts.push(source.substring(i, call.start));
    }

    parts.push(<RefLink call={call} fnNameToFile={fnNameToFile} />);
    i = call.end;
  });
  if (i < source.length) {
    parts.push(source.substring(i));
  }

  return (
    <div>
      <pre>{parts}</pre>
    </div>
  );
};

LinkedCode.propTypes = {
  source: PropTypes.string
};

export default LinkedCode;
