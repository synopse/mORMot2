// mORMot2 Angular Sample - Frontend JavaScript
// Demonstrates CORS-enabled REST API calls to mORMot2 backend

const API_BASE_URL = 'http://localhost:8080';

let customers = [];
let editingCustomerId = null;

// Initialize app on load
document.addEventListener('DOMContentLoaded', () => {
    loadCustomers();
});

// Show status message
function showStatus(message, type = 'info') {
    const status = document.getElementById('status');
    status.textContent = message;
    status.className = `status ${type}`;
    status.style.display = 'block';

    setTimeout(() => {
        status.style.display = 'none';
    }, 5000);
}

// Call mORMot2 JSON-RPC API
async function callApi(method, params = {}) {
    try {
        const response = await fetch(`${API_BASE_URL}/CustomerApi.${method}`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(params)
        });

        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }

        const data = await response.json();

        // mORMot2 JSON-RPC returns {"result":...} or {"error":...}
        if (data.error) {
            throw new Error(data.error.message || 'API error');
        }

        return data.result;
    } catch (error) {
        console.error('API call failed:', error);
        showStatus(`Error: ${error.message}`, 'error');
        throw error;
    }
}

// Load all customers
async function loadCustomers() {
    try {
        showStatus('Loading customers...', 'info');
        customers = await callApi('GetAll');
        renderCustomers(customers);
        showStatus(`Loaded ${customers.length} customers`, 'success');
    } catch (error) {
        document.getElementById('customerTable').innerHTML =
            '<div class="error">Failed to load customers. Make sure the server is running.</div>';
    }
}

// Search customers
async function searchCustomers() {
    const query = document.getElementById('searchInput').value.trim();

    if (!query) {
        loadCustomers();
        return;
    }

    try {
        showStatus('Searching...', 'info');
        customers = await callApi('Search', { query });
        renderCustomers(customers);
        showStatus(`Found ${customers.length} customers`, 'success');
    } catch (error) {
        // Error already shown by callApi
    }
}

// Render customers table
function renderCustomers(customers) {
    const tableDiv = document.getElementById('customerTable');

    if (customers.length === 0) {
        tableDiv.innerHTML = '<div class="empty">No customers found</div>';
        return;
    }

    let html = `
        <table>
            <thead>
                <tr>
                    <th>ID</th>
                    <th>Name</th>
                    <th>Email</th>
                    <th>Phone</th>
                    <th>City</th>
                    <th>Country</th>
                    <th>Actions</th>
                </tr>
            </thead>
            <tbody>
    `;

    customers.forEach(customer => {
        html += `
            <tr>
                <td>${customer.ID}</td>
                <td>${customer.FirstName} ${customer.LastName}</td>
                <td>${customer.Email}</td>
                <td>${customer.Phone || '-'}</td>
                <td>${customer.City || '-'}</td>
                <td>${customer.Country || '-'}</td>
                <td>
                    <div class="actions">
                        <button class="btn-small btn-edit" onclick="editCustomer(${customer.ID})">Edit</button>
                        <button class="btn-small btn-delete" onclick="deleteCustomer(${customer.ID})">Delete</button>
                    </div>
                </td>
            </tr>
        `;
    });

    html += '</tbody></table>';
    tableDiv.innerHTML = html;
}

// Show add modal
function showAddModal() {
    editingCustomerId = null;
    document.getElementById('modalTitle').textContent = 'Add Customer';
    document.getElementById('customerForm').reset();
    document.getElementById('customerId').value = '';
    document.getElementById('customerModal').style.display = 'block';
}

// Edit customer
async function editCustomer(id) {
    try {
        const customer = await callApi('GetById', { id });

        editingCustomerId = id;
        document.getElementById('modalTitle').textContent = 'Edit Customer';
        document.getElementById('customerId').value = customer.ID;
        document.getElementById('firstName').value = customer.FirstName;
        document.getElementById('lastName').value = customer.LastName;
        document.getElementById('email').value = customer.Email;
        document.getElementById('phone').value = customer.Phone || '';
        document.getElementById('city').value = customer.City || '';
        document.getElementById('country').value = customer.Country || '';

        document.getElementById('customerModal').style.display = 'block';
    } catch (error) {
        // Error already shown by callApi
    }
}

// Save customer (add or update)
async function saveCustomer(event) {
    event.preventDefault();

    const customer = {
        FirstName: document.getElementById('firstName').value,
        LastName: document.getElementById('lastName').value,
        Email: document.getElementById('email').value,
        Phone: document.getElementById('phone').value,
        City: document.getElementById('city').value,
        Country: document.getElementById('country').value
    };

    try {
        if (editingCustomerId) {
            // Update existing
            await callApi('Update', { id: editingCustomerId, customer });
            showStatus('Customer updated successfully', 'success');
        } else {
            // Create new
            const newId = await callApi('CreateCustomer', { customer });
            showStatus(`Customer created successfully (ID: ${newId})`, 'success');
        }

        closeModal();
        loadCustomers();
    } catch (error) {
        // Error already shown by callApi
    }
}

// Delete customer
async function deleteCustomer(id) {
    if (!confirm('Are you sure you want to delete this customer?')) {
        return;
    }

    try {
        await callApi('Delete', { id });
        showStatus('Customer deleted successfully', 'success');
        loadCustomers();
    } catch (error) {
        // Error already shown by callApi
    }
}

// Close modal
function closeModal() {
    document.getElementById('customerModal').style.display = 'none';
    editingCustomerId = null;
}

// Close modal on outside click
window.onclick = function(event) {
    const modal = document.getElementById('customerModal');
    if (event.target === modal) {
        closeModal();
    }
}

// Search on Enter key
document.addEventListener('DOMContentLoaded', () => {
    const searchInput = document.getElementById('searchInput');
    searchInput.addEventListener('keypress', (event) => {
        if (event.key === 'Enter') {
            searchCustomers();
        }
    });
});
