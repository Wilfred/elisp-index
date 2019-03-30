import React from "react";
import { Link, graphql } from "gatsby";

import Layout from "../components/layout";
import SEO from "../components/seo";

const LispFile = ({ data }) => {
  console.log([data]);
  return (
    <Layout>
      <SEO title={data.lispJson.name} />
      <h1>{data.lispJson.name}</h1>
      <Link to="/">Go back to the homepage</Link>
    </Layout>
  );
};

export default LispFile;

export const query = graphql`
  query($name: String!) {
    lispJson(name: { eq: $name }) {
      name
      source
    }
  }
`;
