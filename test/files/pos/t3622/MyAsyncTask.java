package test;

public abstract class MyAsyncTask extends AsyncTask<String, String, String> {
    protected abstract String doInBackground1(String[] args);
    @Override
    protected String doInBackground(String... args) {
        return doInBackground1(new String[]{"dummy"});
    }
}