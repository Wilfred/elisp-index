import PropTypes from "prop-types";
import React from "react";

const LinkedCode = ({ source, calls }) => {
  let parts = [];
  let i = 0;
  calls.forEach(call => {
    if (call.start > i) {
      parts.push(source.substring(i, call.start));
    }
    let url = "#" + call.name;
    let key = call.name + call.start;
    parts.push(
      <a className="call" key={key} href={url}>
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
