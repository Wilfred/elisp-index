import PropTypes from "prop-types";
import React from "react";

const LinkedCode = ({ source, calls, fnNameToFile }) => {
  let parts = [];
  let i = 0;
  calls.forEach(call => {
    if (call.start > i) {
      parts.push(source.substring(i, call.start));
    }

    let url = "#";
    let className = "has-text-danger";
    if (fnNameToFile[call.name]) {
      url = "/file/" + fnNameToFile[call.name];
      className = "";
    }

    // TODO: use a proper Link component.
    parts.push(
      <a className={className} key={call.name + call.start} href={url}>
        {call.name}
      </a>
    );
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
