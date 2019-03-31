import React from "react";
import { Link, graphql } from "gatsby";

import Layout from "../components/layout";
import LinkedCode from "../components/linkedcode";
import SEO from "../components/seo";

const LispFile = ({ data, pageContext }) => {
  return (
    <Layout>
      <SEO title={data.lispJson.name} />
      <h1 className="title">{data.lispJson.name}</h1>
      <LinkedCode
        source={data.lispJson.source}
        calls={data.lispJson.calls}
        fnNameToFile={pageContext.funNameToFile}
      />
    </Layout>
  );
};

export default LispFile;

export const query = graphql`
  query($name: String!) {
    lispJson(name: { eq: $name }) {
      name
      source
      calls {
        start
        end
        name
      }
    }
  }
`;
