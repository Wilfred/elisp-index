import React from "react";
import { graphql } from "gatsby";

import Layout from "../components/layout";
import LinkedCode from "../components/linkedcode";
import SEO from "../components/seo";

const IndexPage = ({ data }) => {
  let fileLinks = data.allLispJson.edges.map(e => (
    <li>
      <a href="#">{e.node.name}</a>
    </li>
  ));

  return (
    <Layout>
      <SEO title="Home" keywords={[`gatsby`, `application`, `react`]} />
      <ul>{fileLinks}</ul>
    </Layout>
  );
};

export default IndexPage;

export const query = graphql`
  {
    allLispJson {
      edges {
        node {
          name
        }
      }
    }
  }
`;
