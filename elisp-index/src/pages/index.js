import React from "react";
import { graphql } from "gatsby";

import Layout from "../components/layout";
import SEO from "../components/seo";

const IndexPage = ({ data }) => {
  const functions = data.dataJson.functions.map(fn => (
    <li key={fn.name}>
      {fn.name}: {fn.position}
    </li>
  ));
  return (
    <Layout>
      <SEO title="Home" keywords={[`gatsby`, `application`, `react`]} />
      <ol>{functions}</ol>
    </Layout>
  );
};

export default IndexPage;

export const query = graphql`
  {
    dataJson {
      functions {
        position
        name
      }
    }
  }
`;
