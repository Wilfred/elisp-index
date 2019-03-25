import React from "react";
import { graphql } from "gatsby";

import Layout from "../components/layout";
import SEO from "../components/seo";

const IndexPage = ({ data }) => {
  const functions = data.dataJson.functions.map(fn => (
    <li key={fn.name}>
      {fn.name}: {fn.start}-{fn.end}
    </li>
  ));
  return (
    <Layout>
      <SEO title="Home" keywords={[`gatsby`, `application`, `react`]} />
      <pre>
        <code>{data.dataJson.source}</code>
      </pre>
      <ol>{functions}</ol>
    </Layout>
  );
};

export default IndexPage;

export const query = graphql`
  {
    dataJson {
      source
      functions {
        start
        end
        name
      }
    }
  }
`;
