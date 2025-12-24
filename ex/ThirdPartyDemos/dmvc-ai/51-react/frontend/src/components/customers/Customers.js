import React, { useEffect, useState } from 'react';
import api from '../../services/api';
import {useNavigate} from 'react-router-dom';

function Customers(props) {

    const [state, setState] = useState({ customers: [] });
    const navigate = useNavigate();

    function handleAdd() {
        navigate("/customer");
    }

    async function handleDelete(id) {
        await api.delete(`/customers/${id}`);
        const response = await api.get('/customers');
        setState({ customers: response.data });

    }

    useEffect(() => {
        api.get('/customers')
            .then(customers => {
                setState({ customers: customers.data });
            });

    }, []);

    return (
        <>
            <table className="table">
                <thead>
                    <tr>
                        <th scope="col">#</th>
                        <th scope="col">Code</th>
                        <th scope="col">Description</th>
                        <th scope="col">City</th>
                        <th scope="col">Rating</th>
                        <th scope="col">Operations</th>
                    </tr>
                </thead>
                <tbody>
                    {state.customers.map(customer => (
                        <tr key={customer.ID}>
                            <td>{customer.ID}</td>
                            <td>{customer.Code}</td>
                            <td>{customer.Description}</td>
                            <td>{customer.City}</td>
                            <td>{customer.Rating}</td>
                            <td>
                                <button onClick={() => navigate(`/customer/${customer.ID}`)} className="btn  btn-sm btn-success">Edit</button>
                                <button onClick={() => handleDelete(customer.ID)} className="btn  btn-sm btn-warning">Delete</button>
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </>
    )
}


export default Customers;
