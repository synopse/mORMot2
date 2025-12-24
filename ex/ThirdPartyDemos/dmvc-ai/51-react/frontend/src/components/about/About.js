import React from 'react';

function About() {
    return (
        <div className="container">
            <h2>About mORMot2 React Sample</h2>
            <p>
                This is a demonstration of mORMot2 REST API integration with React frontend.
            </p>
            <h3>Features</h3>
            <ul>
                <li>RESTful API with mORMot2 backend</li>
                <li>React frontend with React Router</li>
                <li>CRUD operations for Customer entity</li>
                <li>Form validation and error handling</li>
                <li>CORS support for cross-origin requests</li>
            </ul>
            <h3>Technology Stack</h3>
            <ul>
                <li><strong>Backend:</strong> mORMot2 (Delphi)</li>
                <li><strong>Frontend:</strong> React 16.13</li>
                <li><strong>HTTP Client:</strong> Axios</li>
                <li><strong>Routing:</strong> React Router 6</li>
                <li><strong>UI Framework:</strong> Bootstrap 4</li>
            </ul>
            <h3>API Endpoints</h3>
            <ul>
                <li>GET /api/customers - List all customers</li>
                <li>GET /api/customers/:id - Get customer by ID</li>
                <li>POST /api/customers - Create new customer</li>
                <li>PUT /api/customers/:id - Update customer</li>
                <li>DELETE /api/customers/:id - Delete customer</li>
            </ul>
            <p>
                <strong>Source:</strong> Port of DelphiMVCFramework React sample to mORMot2
            </p>
        </div>
    );
}

export default About;
